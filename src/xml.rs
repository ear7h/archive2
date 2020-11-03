use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt,
    fs::File,
    io,
    io::BufReader,
    rc::{Rc, Weak},
};

use xml::{
    attribute::OwnedAttribute,
    name::OwnedName,
    namespace::Namespace,
    reader::{Error as XmlError, EventReader, Events, Result as XmlResult, XmlEvent},
};

struct Node {
    parent: Option<Weak<RefCell<Node>>>,
    name: OwnedName,
    attributes: Vec<OwnedAttribute>,
    namespace: Namespace,
    children: Vec<Tree>,
}

impl Node {
    fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    fn add_child(&mut self, tree: Tree) {
        self.children.push(tree);
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "<{}", self.name)?;
        for attr in self.attributes.iter() {
            write!(f, " {}", attr)?;
        }
        write!(f, ">\n")?;

        for child in &self.children {
            child.fmt_rec(f, 1)?;
            write!(f, "\n")?;
        }

        write!(f, "</{}>", self.name)
    }
}

#[derive(Clone)]
enum Tree {
    Node(Rc<RefCell<Node>>),
    Text(Option<Weak<RefCell<Node>>>, String),
}

impl Tree {
    fn is_root(&self) -> bool {
        match self {
            Tree::Node(node) => node.borrow().is_root(),
            Tree::Text(parent, _) => parent.is_none(),
        }
    }

    fn from_node_rc(node: &Rc<RefCell<Node>>) -> Self {
        Tree::Node(Rc::clone(node))
    }

    fn fmt_rec(&self, f: &mut fmt::Formatter<'_>, depth: usize) -> Result<(), fmt::Error> {
        for _ in 0..depth {
            write!(f, "\t")?;
        }

        match self {
            Tree::Node(node) => {
                write!(f, "<{}", node.borrow().name)?;
                for attr in node.borrow().attributes.iter() {
                    write!(f, "{}", attr)?;
                }
                write!(f, ">\n")?;

                for child in &node.borrow().children {
                    child.fmt_rec(f, depth + 1)?;
                    write!(f, "\n")?;
                }

                for _ in 0..depth {
                    write!(f, "\t")?;
                }
                write!(f, "</{}>", node.borrow().name)
            }
            Tree::Text(_, text) => write!(f, "{}", text),
        }
    }
}

impl fmt::Display for Tree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.fmt_rec(f, 0)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum TreeStep {
    Revisit,
    Next, //  go down or to the next sibling or up
    Skip, // go to the next sibling or up if there are none
    Up,   // skip all the siblings
}

impl TreeStep {
    fn return_next<T>(&self, next: (TreeStep, Option<T>)) -> (TreeStep, Option<T>) {
        let (step, val) = next;
        (*self.min(&step), val)
    }
}

#[derive(PartialEq, Eq)]
enum VisitType {
    Pre,
    Post,
    PostNoSave,
}

pub struct Return<T> {
    f: Box<dyn Fn(&mut WalkState, bool) -> Option<Option<T>>>,
}

impl<T> Return<T>
where
    T: 'static,
{
    fn from_lambda<F>(f: F) -> Self
    where
        F: Fn(&mut WalkState, bool) -> Option<Option<T>> + 'static,
    {
        Self { f: Box::new(f) }
    }
}

impl<T> Return<T>
where
    T: Clone + 'static,
{
    fn constant(val: T) -> Self {
        Self::from_lambda(move |state, revisit| Some(Some(val.clone())))
    }
}

impl Return<()> {
    fn nil() -> Self {
        Self::from_lambda(|state, revisit| Some(None))
    }
}

impl Return<String> {
    pub fn outer_html() -> Self {
        Self::from_lambda(|state, revisit| {
            if !revisit {
                state.mark_revisit(true);
                Some(None)
            } else {
                Some(Some(format!("{}", state.current())))
            }
        })
    }
}

impl Return<Vec<String>> {
    pub fn attribute(s: String) -> Self {
        Self::from_lambda(move |state, revisit| {
            let attrs = state
                .current()
                .attributes
                .iter()
                .filter_map(|attr| {
                    if attr.name.local_name == s {
                        Some(attr.value.clone())
                    } else {
                        None
                    }
                })
                .collect();

            Some(Some(attrs))
        })
    }
}
impl Return<Vec<(String, String)>> {
    pub fn attributes() -> Self {
        Self::from_lambda(|state, revisit| {
            let attrs = state
                .current()
                .attributes
                .iter()
                .map(|attr| (attr.name.local_name.clone(), attr.value.clone()))
                .collect();

            Some(Some(attrs))
        })
    }
}

pub struct Matcher<T> {
    // takse a state and revisit
    // returns
    //      None if eof
    //      Some(None) if no match
    //      Some(Some(T)) if match
    f: Box<dyn Fn(&mut WalkState, bool) -> Option<Option<T>>>,
}

impl<T> Matcher<T>
where
    T: 'static,
{
    pub fn new(r: Return<T>) -> Self {
        Self {
            f: Box::new(move |state, revisit| (r.f)(state, revisit)),
        }
    }

    fn from_lambda<F>(f: F) -> Self
    where
        F: Fn(&mut WalkState, bool) -> Option<Option<T>> + 'static,
    {
        Self { f: Box::new(f) }
    }

    pub fn tag_eq(self, s: String) -> Self {
        Self::from_lambda(move |state, revisit| {
            if let Some(rc) = &state.current {
                if rc.borrow().name.local_name == s {
                    return (self.f)(state, revisit);
                }
            }

            Some(None)
        })
    }

    pub fn has_attr(self, name: String) -> Self {
        Self::from_lambda(move |state, revisit| {
            let exists = state
                .current()
                .attributes
                .iter()
                .find(|attr| attr.name.local_name == name)
                .is_some();

            if exists {
                return (self.f)(state, revisit);
            }

            Some(None)
        })
    }

    pub fn attr_eq(self, name: String, val: String) -> Self {
        Self::from_lambda(move |state, revisit| {
            let exists = state
                .current()
                .attributes
                .iter()
                .find(|attr| attr.name.local_name == name && attr.value == val)
                .is_some();
            if exists {
                return (self.f)(state, revisit);
            }

            Some(None)
        })
    }

    pub fn root(self) -> Self {
        Self::from_lambda(move |state, revisit| {
            if state.current().is_root() {
                return (self.f)(state, revisit);
            }
            Some(None)
        })
    }

    pub fn wild(self) -> Self {
        Self::from_lambda(move |state, revisit| {
            match (self.f)(state, revisit) {
                None => return None,
                Some(None) => {}
                Some(Some(val)) => return Some(Some(val)),
            }

            while let Some(revisit) = state.step() {
                match (self.f)(state, revisit) {
                    None => return None,
                    Some(None) => {}
                    Some(Some(val)) => return Some(Some(val)),
                }
            }

            None
        })
    }

    pub fn and<U: 'static>(self, next: Matcher<U>) -> Matcher<U> {
        Matcher::from_lambda(move |state, revisit| match (self.f)(state, revisit) {
            None => None,
            Some(None) => Some(None),
            Some(Some(_)) => match state.step() {
                None => None,
                Some(revisit) => (next.f)(state, revisit),
            },
        })
    }

    pub fn and_then<U: 'static, F>(self, next: F) -> Matcher<U>
    where
        F: Fn(T) -> Matcher<U> + 'static,
    {
        Matcher::from_lambda(move |state, revisit| match (self.f)(state, revisit) {
            None => None,
            Some(None) => Some(None),
            Some(Some(val)) => match state.step() {
                None => None,
                Some(revisit) => (next(val).f)(state, revisit),
            },
        })
    }
}

impl<T> Matcher<T>
where
    T: 'static,
{
    pub fn or(self, next: Self) -> Self {
        Self::from_lambda(move |state, revisit| {
            let mut cloned = state.clone();
            match (self.f)(&mut cloned, revisit) {
                None => None,
                Some(Some(val)) => {
                    *state = cloned;
                    Some(Some(val))
                }
                Some(None) => (next.f)(state, revisit),
            }
        })
    }

    pub fn or_else<F>(self, next: F) -> Self
    where
        F: Fn() -> Matcher<T> + 'static,
    {
        Self::from_lambda(move |state, revisit| {
            let mut cloned = state.clone();
            match (self.f)(&mut cloned, revisit) {
                None => None,
                Some(Some(val)) => {
                    *state = cloned;
                    Some(Some(val))
                }
                Some(None) => (next().f)(state, revisit),
            }
        })
    }
}

impl<T> Matcher<T>
where
    T: 'static,
{
    pub fn matches<R: io::Read + 'static>(self, r: EventReader<R>) -> MatchIter<T> {
        MatchIter {
            m: self,
            s: WalkState {
                r: Backtracker::new(Box::new(r.into_iter())),
                root: None,
                current: None,
                depth: 0,
                revisits: Vec::new(),
            },
        }
    }
}

struct WalkState {
    // this doesn't have to be Rc it's a hack for garbage collection in the
    // BacktrackerBuf
    r: Rc<RefCell<Backtracker<Box<dyn Iterator<Item = XmlResult<XmlEvent>>>, XmlResult<XmlEvent>>>>,
    root: Option<Rc<RefCell<Node>>>,
    current: Option<Rc<RefCell<Node>>>,
    depth: usize,
    revisits: Vec<usize>,
}

impl WalkState {
    fn clone(&mut self) -> Self {
        Self {
            r: self.r.clone(),
            root: self.root.as_ref().map(|rc| Rc::clone(rc)),
            current: self.current.as_ref().map(|rc| Rc::clone(rc)),
            depth: self.depth,
            revisits: self.revisits.clone(),
        }
    }

    fn current(&self) -> std::cell::Ref<Node> {
        self.current.as_ref().unwrap().borrow()
    }

    /// reads the next event and returns the (possibly incomplete)
    /// Tree representing the newly read node and bool if this
    /// is a revist
    ///
    /// None return value indicates there is nothing more to read
    fn step(&mut self) -> Option<bool> {
        loop {
            let opt = self.r.borrow_mut().next();
            if opt.is_none() {
                return None;
            }

            let evt = opt.unwrap().unwrap();

            match evt {
                XmlEvent::StartElement {
                    name,
                    attributes,
                    namespace,
                } => {
                    let node = Node {
                        parent: self.current.as_ref().map(|rc| Rc::downgrade(rc)),
                        name: name,
                        attributes: attributes,
                        namespace: namespace,
                        children: Vec::new(),
                    };

                    let rc = Rc::new(RefCell::new(node));
                    let tree = Tree::Node(Rc::clone(&rc));

                    if let Some(current) = self.current.as_mut() {
                        current.borrow_mut().add_child(tree.clone());
                    }

                    self.current = Some(Rc::clone(&rc));
                    self.depth += 1;

                    return Some(false);
                }
                XmlEvent::EndElement { name } => {
                    if self.depth > 0 {
                        self.depth -= 1;

                        self.current.as_ref().map(|rc| {
                            if rc.borrow().name != name {
                                println!("{} {}", "unmatched end tag", name);
                            }
                        });

                        self.current = self.current.as_ref().and_then(|rc| {
                            rc.borrow()
                                .parent
                                .as_ref()
                                .and_then(|weak| Weak::upgrade(weak))
                        });
                    } else {
                        println!("{} {}", "unmatched end tag", name);
                    }

                    if self.should_revisit() {
                        self.did_revisit();
                        return Some(true);
                    }
                }
                XmlEvent::Characters(s) => {
                    let tree = Tree::Text(self.current.as_ref().map(|rc| Rc::downgrade(rc)), s);
                    if let Some(current) = self.current.as_mut() {
                        current.borrow_mut().add_child(tree);
                    }
                }
                XmlEvent::EndDocument => return None,
                XmlEvent::StartDocument { .. } => {}
                _ => {}
            }
        }
    }

    /// step MUST BE CALLED AFTER should_revisit
    fn should_revisit(&mut self) -> bool {
        let ret = self
            .revisits
            .last()
            .map_or(false, |depth| *depth == self.depth);

        if ret {
            assert!(
                self.current.is_some(),
                "we probably fucked up the ref counting"
            )
        }

        ret
    }

    fn did_revisit(&mut self) {
        let hasnt_revisited = self
            .revisits
            .last()
            .map_or(false, |depth| *depth == self.depth);
        if hasnt_revisited {
            self.revisits.pop();
        }
    }

    /// step MUST BE CALLED BEFORE mark_revisit OR
    fn mark_revisit(&mut self, should_save: bool) {
        let already_set = self
            .revisits
            .last()
            .map_or(false, |depth| *depth == self.depth);
        if !already_set {
            self.revisits.push(self.depth);

            if self.root.is_none() && should_save {
                let rc = self.current.as_ref().unwrap();
                self.root = Some(Rc::clone(&rc))
            }
        }
    }
}

pub struct MatchIter<T> {
    s: WalkState,
    m: Matcher<T>,
}

impl<T> Iterator for MatchIter<T>
where
    T: 'static,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(revisit) = self.s.step() {
            match (self.m.f)(&mut self.s, revisit) {
                None => return None,
                Some(None) => {}
                Some(Some(val)) => return Some(val),
            }
        }
        None
    }
}

struct BacktrackerBuf<I, T> {
    src: I,
    clients: Vec<Weak<RefCell<Backtracker<I, T>>>>,
    cursor: usize,
    buf: VecDeque<T>,
}

impl<I, T> BacktrackerBuf<I, T>
where
    T: Clone,
    I: Iterator<Item = T>,
{
    fn next(&mut self, client: &Backtracker<I, T>) -> Option<T> {
        let pos = client.pos;
        let idx = client.pos - self.cursor;

        if idx >= self.buf.len() {
            match self.src.next() {
                None => return None,
                Some(val) => self.buf.push_back(val),
            }
        }

        self.clients.retain(|weak| weak.strong_count() > 0);

        let min = self
            .clients
            .iter()
            .map(|weak| {
                if (client as *const Backtracker<I, T>) == Weak::upgrade(weak).unwrap().as_ptr() {
                    client.pos
                } else {
                    Weak::upgrade(weak).unwrap().borrow().pos
                }
            })
            .min()
            .unwrap();

        if idx == 0 && pos < min {
            assert!(pos + 1 == min);

            self.cursor = min;
            return Some(self.buf.pop_front().unwrap());
        }

        Some(self.buf[idx].clone())
    }
}

pub struct Backtracker<I, T> {
    pos: usize,
    buf: Rc<RefCell<BacktrackerBuf<I, T>>>,
}

impl<I, T> Iterator for Backtracker<I, T>
where
    T: Clone + fmt::Debug,
    I: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.buf.borrow_mut().next(self);
        self.pos += 1;
        ret
    }
}

impl<I, T> Backtracker<I, T>
where
    I: Iterator<Item = T>,
{
    fn new(iter: I) -> Rc<RefCell<Self>> {
        let ret = Rc::new(RefCell::new(Self {
            pos: 0,
            buf: Rc::new(RefCell::new(BacktrackerBuf {
                src: iter,
                clients: Vec::new(),
                cursor: 0,
                buf: VecDeque::new(),
            })),
        }));

        let weak = Rc::downgrade(&ret);
        ret.borrow_mut().buf.borrow_mut().clients.push(weak);
        ret
    }

    fn clone(&self) -> Rc<RefCell<Self>> {
        let ret = Rc::new(RefCell::new(Self {
            pos: self.pos,
            buf: Rc::clone(&self.buf),
        }));

        let weak = Rc::downgrade(&ret);
        ret.borrow_mut().buf.borrow_mut().clients.push(weak);
        ret
    }
}
