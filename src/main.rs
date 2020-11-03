#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![feature(trait_alias)]

use std::{collections::HashMap, fs, io, io::Seek, path, path::Path, path::PathBuf};

use ureq;
use url::Url;

use ::xml::{
    attribute::OwnedAttribute,
    name::OwnedName,
    namespace::Namespace,
    reader::{EventReader, XmlEvent},
};

mod xml;

struct Artifact<B> {
    timestamp: u64, // unix time
    url: Url,
    mime: String,
    id: Vec<u8>,
    parent_id: Option<Vec<u8>>,
    body: B,
}

impl<B> Artifact<B> {
    fn with_new_body<B1>(self, b: B1) -> Artifact<B1> {
        Artifact {
            timestamp: self.timestamp,
            url: self.url,
            mime: self.mime,
            id: self.id,
            parent_id: self.parent_id,
            body: b,
        }
    }
}

#[derive(Debug)]
enum Error {
    Io(io::Error),
    Ureq(String), // genius ureq doesn't give a way to own the request error
    NotImplemented,
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}

type Result<T> = std::result::Result<T, Error>;

struct FsArchive {
    dir: PathBuf,
}

impl FsArchive {
    fn new<P: AsRef<Path>>(dir: P) -> Result<Self> {
        let pb = dir.as_ref().to_path_buf();
        fs::create_dir_all(pb.as_path())?;

        Ok(Self { dir: pb })
    }
}

impl Archive for FsArchive {
    type BackingFile = fs::File;

    fn catalog<R: io::Read>(&mut self, mut a: Artifact<R>) -> Result<Artifact<Self::BackingFile>> {
        let mut pb = self.dir.clone();
        pb.push(format!("{}", a.timestamp));
        let u = &a.url;
        pb.push(u.scheme());
        pb.push(u.host_str().unwrap());
        pb.push(format!("{}", u.port_or_known_default().unwrap()));

        let before = pb.as_os_str().len();

        if let Some(segs) = u.path_segments() {
            for seg in segs {
                if seg != "" {
                    pb.push(seg)
                }
            }
        }

        if pb.as_os_str().len() == before || pb.extension() == None {
            pb.push("INDEX")
        }

        fs::create_dir_all(pb.parent().unwrap())?;
        let mut file = fs::OpenOptions::new()
            .write(true)
            .read(true)
            .create(true)
            .truncate(true)
            .open(pb)?;

        io::copy(&mut a.body, &mut file)?;

        file.sync_all()?;
        file.seek(io::SeekFrom::Start(0))?;

        Ok(a.with_new_body(file))
    }

    fn catalog_final<R: io::Read>(&mut self, a: Artifact<R>) -> Result<Vec<u8>> {
        let art = self.catalog(a)?;
        Ok(art.id)
    }
}

trait Archive {
    type BackingFile: io::Read; // + io::Write

    fn catalog<R: io::Read>(&mut self, a: Artifact<R>) -> Result<Artifact<Self::BackingFile>>;

    fn catalog_final<R: io::Read>(&mut self, a: Artifact<R>) -> Result<Vec<u8>>;
}

struct Archiver<T, A: Archive> {
    f: Box<dyn FnOnce(&mut A) -> Result<T>>,
}

impl<T: 'static, A: Archive + 'static> Archiver<T, A> {
    fn run(self, arc: &mut A) -> Result<T> {
        (self.f)(arc)
    }

    fn ret(t: T) -> Self {
        Self {
            f: Box::new(|arc| Ok(t)),
        }
    }

    fn catalog<R>(self) -> Archiver<Artifact<A::BackingFile>, A>
    where
        R: io::Read,
        T: Into<Artifact<R>>,
    {
        Archiver {
            f: Box::new(move |arc| {
                let inner = (self.f)(arc)?;
                let art = inner.into();
                arc.catalog(art)
            }),
        }
    }

    fn catalog_final<R>(self) -> Archiver<(), A>
    where
        R: io::Read,
        T: Into<Artifact<R>>,
    {
        Archiver {
            f: Box::new(move |arc| {
                let art = (self.f)(arc)?.into();
                arc.catalog_final(art)?;
                Ok(())
            }),
        }
    }

    fn fmap<U, F>(self, f: F) -> Archiver<U, A>
    where
        F: FnOnce(T) -> U + 'static,
    {
        Archiver {
            f: Box::new(|arc| {
                let v = (self.f)(arc)?;
                Ok(f(v))
            }),
        }
    }

    fn or(self, a: Archiver<T, A>) -> Archiver<T, A> {
        Archiver {
            f: Box::new(|arc| (self.f)(arc).or_else(|_| (a.f)(arc))),
        }
    }

    fn or_else<F: 'static>(self, f: F) -> Archiver<T, A>
    where
        F: FnOnce() -> Archiver<T, A>,
    {
        Archiver {
            f: Box::new(|arc| (self.f)(arc).or_else(|_| (f().f)(arc))),
        }
    }

    fn and<U: 'static>(self, a: Archiver<U, A>) -> Archiver<U, A> {
        Archiver {
            f: Box::new(|arc| {
                (self.f)(arc)?;
                (a.f)(arc)
            }),
        }
    }

    fn and_then<U, F: 'static>(self, f: F) -> Archiver<U, A>
    where
        F: FnOnce(T) -> Archiver<U, A>,
    {
        Archiver {
            f: Box::new(|arc| {
                let t = (self.f)(arc)?;
                ((f(t)).f)(arc)
            }),
        }
    }

    // this is like haskell's mapM
    fn and_then_iter<It, Iu, F: 'static>(self, f: F) -> Archiver<Vec<Iu>, A>
    where
        T: Iterator<Item = It>,
        F: Fn(It) -> Archiver<Iu, A>,
    {
        Archiver {
            f: Box::new(move |arc| {
                let mut ret: Vec<Iu> = Vec::new();

                let it = (self.f)(arc)?;

                for v in it {
                    let vv = (f(v).f)(arc)?;
                    ret.push(vv);
                }

                Ok(ret)
            }),
        }
    }

    fn http_get(self) -> Archiver<ureq::Response, A>
    where
        T: AsRef<str>,
    {
        Archiver {
            f: Box::new(|arc| {
                let s = (self.f)(arc)?;

                let res = ureq::get(s.as_ref()).call();
                if res.synthetic() {
                    return Err(Error::Ureq(
                        res.synthetic_error().as_ref().unwrap().to_string(),
                    ));
                }

                Ok(res)
            }),
        }
    }
}

impl Into<Artifact<Box<dyn io::Read>>> for ureq::Response {
    fn into(self) -> Artifact<Box<dyn io::Read>> {
        Artifact {
            timestamp: 0,
            url: Url::parse(self.get_url()).unwrap(),
            mime: self.content_type().to_string(),
            id: vec![],
            parent_id: None,
            body: Box::new(self.into_reader()),
        }
    }
}

fn main() {
    /*
    let mut fs_arc = FsArchive::new("my-archive".to_string()).unwrap();

    Archiver::ret("https://ear7h.net/index.html")
        .http_get()
        .catalog()
        //.map(read_links)
        //.and_then_iter(
            // |link| Archive::new(None).http(link))
        .run(&mut fs_arc).unwrap();


        */
    let buf = io::BufReader::new(
        //"<tag1 attr=\"1\">hello<a href=\"asd\"><tag2 href=\"#hello\">world</tag2></a></tag1>".as_bytes());
        //"<tag1 attr=\"1\">hello<tag2 href=\"#hello\">world</tag2></tag1>".as_bytes());
        "<a href=\"1\"><a href=\"2\"><tag1 attr=\"1\">hello<tag2 href=\"#hello\">world</tag2></tag1></a></a>".as_bytes());
    //"<tag1 attr=\"1\">hello</tag1>".as_bytes());
    //"<tag2 attr=\"1\">hello</tag2>".as_bytes());

    let r = EventReader::new(buf);
    let m = xml::Matcher::new(xml::Return::attributes())
        .tag_eq("tag1".to_string())
        .and(xml::Matcher::new(xml::Return::attributes()).tag_eq("tag2".to_string()))
        .or(xml::Matcher::new(xml::Return::attributes()).tag_eq("a".to_string()))
        .or(xml::Matcher::new(xml::Return::attributes()).tag_eq("tag2".to_string()))
        .wild();

    for val in m.matches(r) {
        println!("LOOP:\n{:?}", val);
    }

    println!("done");
}
