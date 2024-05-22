//! A transcription of types from the [`OpenRPC` Specification](https://spec.open-rpc.org/).
//!
//! > When quoted, the specification will appear as blockquoted text, like so.

use itertools::Itertools as _;
use schemars::schema::Schema;
use semver::Version;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::BTreeMap;
use thiserror::Error;
use url::Url;

/// > This is the root object of the OpenRPC document.
/// > The contents of this object represent a whole OpenRPC document.
/// > How this object is constructed or stored is outside the scope of the OpenRPC Specification.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OpenRPC {
    /// > REQUIRED.
    /// > This string MUST be the semantic version number of the OpenRPC Specification version that the OpenRPC document uses.
    /// > The openrpc field SHOULD be used by tooling specifications and clients to interpret the OpenRPC document.
    /// > This is not related to the API info.version string.
    pub openrpc: Version,
    /// > REQUIRED.
    /// > Provides metadata about the API.
    /// > The metadata MAY be used by tooling as required.
    pub info: Info,
    /// > REQUIRED.
    /// > The available methods for the API.
    /// > While it is required, the array may be empty (to handle security filtering, for example).
    pub methods: Methods,
    /// > An element to hold various schemas for the specification.
    #[serde(default)]
    pub components: Components,
}

/// > The object provides metadata about the API.
/// > The metadata MAY be used by the clients if needed,
/// > and MAY be presented in editing or documentation generation tools for convenience.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]

pub struct Info {
    /// > REQUIRED.
    /// > The title of the application.
    pub title: String,
    /// > A verbose description of the application.
    /// > GitHub Flavored Markdown syntax MAY be used for rich text representation.
    pub description: Option<String>,
    /// > A URL to the Terms of Service for the API.
    /// > MUST be in the format of a URL.
    pub terms_of_service: Option<Url>,
    /// > The contact information for the exposed API.
    pub contact: Option<Contact>,
    /// > The license information for the exposed API.
    pub license: Option<License>,
    /// > REQUIRED.
    /// > The version of the OpenRPC document
    /// > (which is distinct from the OpenRPC Specification version or the API implementation version).
    pub version: String,
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}

/// > Contact information for the exposed API.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Contact {
    /// > The identifying name of the contact person/organization.
    pub name: Option<String>,
    /// > The URL pointing to the contact information.
    /// > MUST be in the format of a URL.
    pub url: Option<Url>,
    /// > The email address of the contact person/organization.
    /// > MUST be in the format of an email address.
    pub email: Option<String>,
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}

/// > License information for the exposed API.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct License {
    /// > REQUIRED.
    /// > The license name used for the API.
    pub name: String,
    /// > A URL to the license used for the API.
    /// > MUST be in the format of a URL.
    pub url: Option<Url>,
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}

// pub struct Server {} // TODO
// pub struct ServerVariable {} // TODO

/// > Describes the interface for the given method name.
/// > The method name is used as the method field of the JSON-RPC body.
/// > It therefore MUST be unique.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct Method {
    /// > REQUIRED.
    /// > The cannonical name for the method.
    /// > The name MUST be unique within the methods array.
    pub name: String,
    /// > A list of tags for API documentation control.
    /// > Tags can be used for logical grouping of methods by resources or any other qualifier.
    pub tags: Vec<ReferenceOr<Tag>>,
    /// > A short summary of what the method does.
    pub summary: Option<String>,
    /// > A verbose explanation of the method behavior.
    /// > GitHub Flavored Markdown syntax MAY be used for rich text representation.
    pub description: Option<String>,
    /// > Additional external documentation for this method.
    pub external_docs: Option<ExternalDocumentation>,
    pub params: Params,
    /// > The description of the result returned by the method.
    /// > If defined, it MUST be a Content Descriptor or Reference Object.
    /// > If undefined, the method MUST only be used as a notification.
    pub result: Option<ContentDescriptor>,
    /// > Declares this method to be deprecated.
    /// > Consumers SHOULD refrain from usage of the declared method.
    /// > Default value is `false`.
    pub deprecated: Option<bool>,
    // pub servers: Option<Vec<Server>>, // TODO
    /// > A list of custom application defined errors that MAY be returned.
    /// > The Errors MUST have unique error codes.
    pub errors: Option<Vec<ReferenceOr<Error>>>,
    // pub links: Option<Vec<ReferenceOr<Link>>>, // TODO
    pub param_structure: Option<ParamStructure>,
    // pub examples: Option<Vec<ReferenceOr<ExamplePairing>>>, // TODO
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContentDescriptor {
    /// > REQUIRED.
    /// > Name of the content that is being described.
    /// > If the content described is a method parameter assignable by-name, this field SHALL define the parameter’s key (ie name).
    pub name: String,
    /// > A short summary of the content that is being described.
    pub summary: Option<String>,
    /// > A verbose explanation of the content descriptor behavior.
    /// > GitHub Flavored Markdown syntax MAY be used for rich text representation.
    pub description: Option<String>,
    /// > Determines if the content is a required field.
    /// > Default value is `false`.
    pub required: Option<bool>,
    /// > REQUIRED.
    /// > Schema that describes the content.
    ///
    /// > The Schema Object allows the definition of input and output data types.
    /// > The Schema Objects MUST follow the specifications outline in the JSON Schema Specification 7 Alternatively,
    /// > any time a Schema Object can be used, a Reference Object can be used in its place.
    /// > This allows referencing definitions instead of defining them inline.
    ///
    /// > This object MAY be extended with Specification Extensions.
    pub schema: Schema,
    /// > Specifies that the content is deprecated and SHOULD be transitioned out of usage.
    /// > Default value is `false`.
    pub deprecated: Option<bool>,
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}

// pub struct ExamplePairing {} // TODO
// pub struct Example {} // TODO
// pub struct Link {} // TODO

/// > Defines an application level error.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Error {
    /// > REQUIRED.
    /// > A Number that indicates the error type that occurred.
    /// > This MUST be an integer.
    /// > The error codes from and including -32768 to -32000 are reserved for pre-defined errors.
    /// > These pre-defined errors SHOULD be assumed to be returned from any JSON-RPC api.
    pub code: i64,
    /// > REQUIRED.
    /// > A String providing a short description of the error.
    /// > The message SHOULD be limited to a concise single sentence.
    pub message: String,
    /// > A Primitive or Structured value that contains additional information about the error.
    /// > This may be omitted.
    /// > The value of this member is defined by the Server (e.g. detailed error information, nested errors etc.).
    pub data: Option<Value>,
}

/// > Holds a set of reusable objects for different aspects of the OpenRPC.
/// > All objects defined within the components object will have no effect on the
/// > API unless they are explicitly referenced from properties outside the components object.
/// > All the fixed fields declared above are objects that MUST use keys that match the regular expression: ^[a-zA-Z0-9\.\-_]+$
// TODO ^^
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]

pub struct Components {
    pub content_descriptors: Option<BTreeMap<String, ContentDescriptor>>,
    pub schemas: Option<BTreeMap<String, Schema>>,
    // pub examples: BTreeMap<String, Example>, // TODO
    // pub links: BTreeMap<String, Link>, // TODO
    pub errors: Option<BTreeMap<String, Error>>,
    // pub example_pairing_objects: BTreeMap<String, ExamplePairing>, // TODO
    pub tags: Option<BTreeMap<String, Tag>>,
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}

/// > Adds metadata to a single tag that is used by the Method Object.
/// > It is not mandatory to have a Tag Object per tag defined in the Method Object instances.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct Tag {
    /// > REQUIRED.
    /// > The name of the tag.
    pub name: String,
    /// > A short summary of the tag.
    pub summary: Option<String>,
    /// > A verbose explanation for the tag.
    /// > GitHub Flavored Markdown syntax MAY be used for rich text representation.
    pub description: Option<String>,
    /// > Additional external documentation for this tag.
    pub external_docs: Option<ExternalDocumentation>,
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}

/// > Allows referencing an external resource for extended documentation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExternalDocumentation {
    /// > A verbose explanation of the target documentation.
    /// > GitHub Flavored Markdown syntax MAY be used for rich text representation.
    pub description: Option<String>,
    /// > The URL for the target documentation.
    /// > Value MUST be in the format of a URL.
    pub url: Url,
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReferenceOr<T> {
    /// > A simple object to allow referencing other components in the specification, internally and externally.
    /// > The Reference Object is defined by JSON Schema and follows the same structure, behavior and rules.
    Reference(String),
    Item(T),
}

/// > While the OpenRPC Specification tries to accommodate most use cases,
/// > additional data can be added to extend the specification at certain points.
/// >
/// > The extensions properties are implemented as patterned fields that are always prefixed by "x-".
/// >
/// > The extensions may or may not be supported by the available tooling,
/// > but those may be extended as well to add requested support
/// > (if tools are internal or open-sourced).
#[derive(Debug, Clone, PartialEq, Serialize, Default)]
#[serde(transparent)]
pub struct SpecificationExtensions(pub BTreeMap<String, Value>);

impl<'de> Deserialize<'de> for SpecificationExtensions {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = BTreeMap<String, Value>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a map with string keys starting with `x-`")
            }

            fn visit_map<A: serde::de::MapAccess<'de>>(
                self,
                mut map: A,
            ) -> Result<Self::Value, A::Error> {
                let mut ret = Self::Value::default();
                loop {
                    match map.next_key::<String>() {
                        Err(_) => (),
                        Ok(None) => break,
                        Ok(Some(key)) if key.starts_with("x-") => {
                            let _ = ret.insert(key, map.next_value()?);
                        }
                        Ok(Some(_)) => {
                            let _ = map.next_value::<serde::de::IgnoredAny>()?;
                        }
                    }
                }

                Ok(ret)
            }
        }
        deserializer.deserialize_any(Visitor).map(Self)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Default)]
#[serde(transparent)]
pub struct Methods {
    inner: Vec<Method>,
}

#[derive(Debug, Clone, Error)]
#[error("{}", .0)]
pub struct MethodsError(String);

impl Methods {
    pub fn new(methods: impl IntoIterator<Item = Method>) -> Result<Self, MethodsError> {
        let inner = methods.into_iter().collect::<Vec<_>>();
        let duplicates = inner
            .iter()
            .map(|it| &it.name)
            .duplicates()
            .collect::<Vec<_>>();
        match duplicates.is_empty() {
            true => Ok(Self { inner }),
            false => Err(MethodsError(format!(
                "the following method names are duplicated: [{}]",
                duplicates.iter().join(", ")
            ))),
        }
    }
}

validated_vec!(Methods => Method);

/// > The expected format of the parameters.
/// > As per the JSON-RPC 2.0 specification,
/// > the params of a JSON-RPC request object may be an array, object, or either
/// > (represented as by-position, by-name, and either respectively).
/// > When a method has a paramStructure value of by-name,
/// > callers of the method MUST send a JSON-RPC request object whose params field is an object.
/// > Further, the key names of the params object MUST be the same as the contentDescriptor.names for the given method.
/// > Defaults to "either".
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
pub enum ParamStructure {
    ByName,
    ByPosition,
    #[default]
    Either,
}

/// > REQUIRED.
/// > A list of parameters that are applicable for this method.
/// > The list MUST NOT include duplicated parameters and therefore require name to be unique.
/// > The list can use the Reference Object to link to parameters that are defined by the Content Descriptor Object.
/// > All optional params (content descriptor objects with “required”: false) MUST be positioned after all required params in the list.
#[derive(Debug, Clone, PartialEq, Serialize, Default)]
#[serde(transparent)]
pub struct Params {
    inner: Vec<ContentDescriptor>,
}

#[derive(Debug, Clone, Error)]
#[error("{}", .0)]
pub struct ParamListError(String);

impl Params {
    pub fn new(
        params: impl IntoIterator<Item = ContentDescriptor>,
    ) -> Result<Self, ParamListError> {
        let params = params.into_iter().collect::<Vec<_>>();
        let duplicates = params
            .iter()
            .map(|it| it.name.as_str())
            .duplicates()
            .collect::<Vec<_>>();
        if !duplicates.is_empty() {
            return Err(ParamListError(format!(
                "The following parameter names are duplicated: [{}]",
                duplicates.join(", ")
            )));
        }
        if let Some((first_opt_ix, first_opt_param)) = params
            .iter()
            .enumerate()
            .find(|(_, it)| !it.required.unwrap_or_default())
        {
            let late_mandatory_params = params
                .iter()
                .enumerate()
                .filter(|(ix, it)| it.required.unwrap_or_default() && *ix > first_opt_ix)
                .map(|(_, it)| it.name.clone())
                .collect::<Vec<_>>();

            if !late_mandatory_params.is_empty() {
                return Err(ParamListError(format!(
                    "Mandatory parameters may not follow optional parameters, \
                            but the optional parameter {} is followed by [{}]",
                    first_opt_param.name,
                    late_mandatory_params.join(", ")
                )));
            }
        };
        Ok(Self { inner: params })
    }
}

validated_vec!(Params => ContentDescriptor);

/// `parent` acts as a `Vec<$child>`, with some internal invariant guarded by
/// `$parent::new()`.
///
/// So you only need to implement `new`, and get `len`, `iter`, `deserialize` for free.
macro_rules! validated_vec {
    ($parent:ty => $child:ty) => {
        impl $parent {
            pub fn empty() -> Self {
                Self::default()
            }
            pub fn just(one: $child) -> Self {
                Self { inner: vec![one] }
            }
            pub fn len(&self) -> usize {
                self.inner.len()
            }
            pub fn is_empty(&self) -> bool {
                self.inner.is_empty()
            }
            pub fn iter(&self) -> std::slice::Iter<'_, $child> {
                self.inner.iter()
            }
        }

        impl IntoIterator for $parent {
            type Item = $child;
            type IntoIter = std::vec::IntoIter<$child>;
            fn into_iter(self) -> Self::IntoIter {
                self.inner.into_iter()
            }
        }

        impl<'a> IntoIterator for &'a $parent {
            type Item = &'a $child;
            type IntoIter = std::slice::Iter<'a, $child>;
            fn into_iter(self) -> Self::IntoIter {
                self.inner.iter()
            }
        }

        impl<'de> Deserialize<'de> for $parent {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                Self::new(Vec::deserialize(deserializer)?).map_err(serde::de::Error::custom)
            }
        }
    };
}

pub(crate) use validated_vec;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(untagged, expecting = "a reference or an item")]
enum _ReferenceOr<T> {
    Reference {
        #[serde(rename = "$ref")]
        reference: String,
    },
    Item(T),
}

impl<T> ReferenceOr<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> ReferenceOr<U> {
        match self {
            ReferenceOr::Reference(it) => ReferenceOr::Reference(it),
            ReferenceOr::Item(it) => ReferenceOr::Item(f(it)),
        }
    }
}

impl<T: Serialize> Serialize for ReferenceOr<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            ReferenceOr::Reference(it) => _ReferenceOr::Reference {
                reference: it.clone(),
            },
            ReferenceOr::Item(it) => _ReferenceOr::Item(it),
        }
        .serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for ReferenceOr<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        _ReferenceOr::deserialize(deserializer).map(|it| match it {
            _ReferenceOr::Reference { reference } => ReferenceOr::Reference(reference),
            _ReferenceOr::Item(it) => ReferenceOr::Item(it),
        })
    }
}
