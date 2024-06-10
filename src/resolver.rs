use core::fmt;
use std::collections::BTreeMap;

use crate::{
    resolved, Components, ContentDescriptor, Error, Example, ExamplePairing, Method, OpenRPC,
    ReferenceOr, Tag,
};

/// A broken [`ReferenceOr`] was found.
///
/// Returned from [`resolve_within`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BrokenReference(pub String);

impl fmt::Display for BrokenReference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("failed to resolve the following `$ref`: ")?;
        f.write_str(self.0.as_str())
    }
}

/// Try and resolve all [`ReferenceOr`]s in the document by looking up
/// references in [`OpenRPC::components`].
///
/// Returns a [`BrokenReference`] on the first [`ReferenceOr::Reference`] with no
/// corresponding entry in [`OpenRPC::components`].
pub fn resolve_within(openrpc: OpenRPC) -> Result<resolved::OpenRPC, BrokenReference> {
    self::openrpc(openrpc).map_err(BrokenReference)
}

fn openrpc(
    OpenRPC {
        openrpc,
        info,
        servers,
        methods,
        components,
        external_docs,
        extensions,
    }: OpenRPC,
) -> Result<resolved::OpenRPC, String> {
    Ok(resolved::OpenRPC {
        openrpc,
        info,
        servers,
        methods: methods
            .into_iter()
            .map(|it| {
                match it {
                    // TODO(aatifsyed): https://github.com/open-rpc/spec/issues/389
                    //                  update this when Components::methods is added
                    ReferenceOr::Reference(it) => Err(it),
                    ReferenceOr::Item(it) => Ok(it),
                }
                .and_then(|it| method(components.as_ref(), it))
            })
            .collect::<Result<_, _>>()?,
        components,
        external_docs,
        extensions,
    })
}

fn method(
    components: Option<&Components>,
    Method {
        name,
        tags,
        summary,
        description,
        external_docs,
        params,
        result,
        deprecated,
        servers,
        errors,
        param_structure,
        examples,
        extensions,
    }: Method,
) -> Result<resolved::Method, String> {
    Ok(resolved::Method {
        name,
        tags: tags
            .map(|it| {
                it.into_iter()
                    .map(|it| resolve(components, it))
                    .collect::<Result<_, _>>()
            })
            .transpose()?,
        summary,
        description,
        external_docs,
        params: params
            .into_iter()
            .map(|it| resolve(components, it))
            .collect::<Result<_, _>>()?,
        result: result.map(|it| resolve(components, it)).transpose()?,
        deprecated,
        servers,
        errors: errors
            .map(|it| {
                it.into_iter()
                    .map(|it| resolve(components, it))
                    .collect::<Result<_, _>>()
            })
            .transpose()?,
        param_structure,
        examples: examples
            .map(|it| {
                it.into_iter()
                    .map(|it| {
                        resolve(components, it).and_then(|it| example_pairing(components, it))
                    })
                    .collect::<Result<_, _>>()
            })
            .transpose()?,
        extensions,
    })
}

fn example_pairing(
    components: Option<&Components>,
    ExamplePairing {
        name,
        description,
        summary,
        params,
        result,
        extensions,
    }: ExamplePairing,
) -> Result<resolved::ExamplePairing, String> {
    Ok(resolved::ExamplePairing {
        name,
        description,
        summary,
        params: params
            .into_iter()
            .map(|it| resolve(components, it))
            .collect::<Result<_, _>>()?,
        result: result.map(|it| resolve(components, it)).transpose()?,
        extensions,
    })
}

trait Resolvable: Sized {
    const KEY: &'static str;
    fn get(components: &Components) -> Option<&BTreeMap<String, Self>>;
}
macro_rules! impl_resolvable {
    ($($ty:ty = $key:literal / $field:ident);* $(;)?) => {
        $(
            impl Resolvable for $ty {
                const KEY: &'static str = $key;
                fn get(components: &Components) -> Option<&BTreeMap<String, Self>> {
                    components.$field.as_ref()
                }
            }
        )*
    };
}
impl_resolvable! {
    ContentDescriptor = "contentDescriptors" / content_descriptors;
    Error = "errors" / errors;
    Example = "examples" / examples;
    ExamplePairing = "examplePairingObjects" / example_pairing_objects;
    Tag = "tags" / tags
}
fn resolve<T: Resolvable + Clone>(
    components: Option<&Components>,
    refr: ReferenceOr<T>,
) -> Result<T, String> {
    match refr {
        ReferenceOr::Reference(it) => {
            let Some(key) = it
                .strip_prefix("#/components/")
                .and_then(|it| it.strip_prefix(T::KEY))
                .and_then(|it| it.strip_prefix('/'))
            else {
                return Err(it);
            };
            components
                .and_then(T::get)
                .and_then(|it| it.get(key))
                .cloned()
                .ok_or(it)
        }
        ReferenceOr::Item(it) => Ok(it),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use serde_json::{json, Value};

    #[track_caller]
    fn do_test(before: Value, after: Expect) {
        after.assert_eq(
            &serde_json::to_string_pretty(
                &resolve_within(serde_json::from_value(before).expect("invalid `before` document"))
                    .expect("resolution failed"),
            )
            .unwrap(),
        );
    }

    #[test]
    fn test() {
        do_test(
            json!(
                {
                    "openrpc": "1.0.0",
                    "info": {
                        "title": "",
                        "version": ""
                    },
                    "methods": [
                        {
                            "name": "fooMethod",
                            "params": [
                                {
                                    "$ref": "#/components/contentDescriptors/fooParam"
                                }
                            ],
                        }
                    ],
                    "components": {
                        "contentDescriptors": {
                            "fooParam": {
                                "name": "fooParam0",
                                "schema": true
                            }
                        }
                    }
                }
            ),
            expect![[r#"
                {
                  "openrpc": "1.0.0",
                  "info": {
                    "title": "",
                    "version": ""
                  },
                  "methods": [
                    {
                      "name": "fooMethod",
                      "params": [
                        {
                          "name": "fooParam0",
                          "schema": true
                        }
                      ]
                    }
                  ],
                  "components": {
                    "contentDescriptors": {
                      "fooParam": {
                        "name": "fooParam0",
                        "schema": true
                      }
                    }
                  }
                }"#]],
        );
    }
}
