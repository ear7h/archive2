# archive2

A functional archiving/etl framework.

The framework is made of two main components `Archiver` and `Archive`.
`Archiver` is a monad that allows for type-level programming when doing
etl. `Archive` is the back-end store where artifacts can be save
with the `Archiver.catalog` method (should prbably be renamed to
"archive").

The folowing example downloads a file and stores
it in the local file system:

```rust
fn main() {

    let mut fs_arc = FsArchive::new("my-archive".to_string()).unwrap();

    Archiver::ret("https://ear7h.net/index.html")
        .http_get()
        .catalog()
        .run(&mut fs_arc).unwrap();


    println!("done");
}
```

This isn't very impresive, but it's readable and maintainable. For example,
adding support for cloud storage, structured data extraction, or logging
is as simple as tweeking or wrapping the existing `FsArchive`

Future functionality that needs to be sorted is crawling and parsing various
file formats to extract structured data.

## xml

In this package there is also a functional xml querying EDSL. The "syntax"
is similar to a parser combinators.

```
fn main() {
    let buf = io::BufReader::new(
        "<tag1 attr=\"1\">hello<tag2 href=\"#hello\">world</tag2></tag1>".as_bytes());

    let r = EventReader::new(buf);
    let m = xml::Matcher::return_attributes()
        .tag_eq("tag2".to_string())
        .or(xml::Matcher::return_attributes()
            .tag_eq("tag1".to_string()));

    for val in m.matches(r) {
        println!("LOOP:\n{:?}", val);
    }
}
```

## TODO
- link crawler

