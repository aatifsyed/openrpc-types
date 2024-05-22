use itertools::Itertools as _;
use pulldown_cmark::{BrokenLink, Event, Options, Tag, TagEnd};

const SPEC_MD: &str = include_str!("../spec.md");

fn main() {
    let mut events = pulldown_cmark::Parser::new_with_broken_link_callback(
        SPEC_MD,
        Options::all(),
        Some(|BrokenLink { reference, .. }| Some((reference.clone(), reference))),
    )
    .peekable();
    while let Some(it) = events.next() {
        if let Event::Start(Tag::Heading { .. }) = it {
            let Some(Event::Text(title)) = events.next() else {
                panic!()
            };
            if let Some(obj) = title.strip_suffix("Object").map(str::trim) {
                dbg!(obj);
                events
                    .find(|it| matches!(it, Event::Start(Tag::Table(_))))
                    .unwrap();
                let table = events
                    .peeking_take_while(|it| !matches!(it, Event::End(TagEnd::Table)))
                    .collect::<Vec<_>>();
                dbg!(table);
            }
        }
    }
}

#[test]
fn test() {
    for event in pulldown_cmark::Parser::new_with_broken_link_callback(
        SPEC_MD,
        Options::all(),
        Some(|BrokenLink { reference, .. }| Some((reference.clone(), reference))),
    ) {
        println!("{:?}", event)
    }
}
