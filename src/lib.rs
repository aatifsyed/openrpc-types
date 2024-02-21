//! A transcription of types from the [`OpenRPC` Specification](https://spec.open-rpc.org/).
//!
//! > When quoted, the specification will appear as blockquoted text, like so.

use itertools::Itertools as _;
use schemars::schema::Schema;
use serde::{Deserialize, Serialize};
use tap::Tap as _;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct Method {
    /// > REQUIRED.
    /// > The cannonical name for the method.
    /// > The name MUST be unique within the methods array.
    pub name: String,
    pub params: Params,
    #[serde(default)]
    pub param_structure: ParamStructure,
    /// > The description of the result returned by the method.
    /// > If defined, it MUST be a Content Descriptor or Reference Object.
    /// > If undefined, the method MUST only be used as a notification.
    pub result: Option<ContentDescriptor>,
}

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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContentDescriptor {
    /// > REQUIRED.
    /// > Name of the content that is being described.
    /// > If the content described is a method parameter assignable by-name, this field SHALL define the parameter’s key (ie name).
    pub name: String,
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
    /// > Determines if the content is a required field. Default value is false.
    #[serde(default)]
    pub required: bool,
}

/// > REQUIRED.
/// > A list of parameters that are applicable for this method.
/// > The list MUST NOT include duplicated parameters and therefore require name to be unique.
/// > The list can use the Reference Object to link to parameters that are defined by the Content Descriptor Object.
/// > All optional params (content descriptor objects with “required”: false) MUST be positioned after all required params in the list.
#[derive(Debug, Clone, PartialEq, Serialize, Default)]
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
            .counts_by(|param| param.name.as_str())
            .tap_mut(|counts| counts.retain(|_, count| *count > 1));
        if !duplicates.is_empty() {
            return Err(ParamListError(format!(
                "The following parameter names are duplicated: [{}]",
                duplicates.into_keys().join(", ")
            )));
        }
        if let Some((first_opt_ix, first_opt_param)) =
            params.iter().enumerate().find(|(_, it)| !it.required)
        {
            let late_mandatory_params = params
                .iter()
                .enumerate()
                .filter(|(ix, it)| it.required && *ix > first_opt_ix)
                .map(|(_, it)| it.name.clone())
                .collect::<Vec<_>>();

            if !late_mandatory_params.is_empty() {
                return Err(ParamListError(
                    String::from("Mandatory parameters may not follow optional parameters,")
                        + &format!(
                            "but the optional parameter {} is followed by [{}]",
                            first_opt_param.name,
                            late_mandatory_params.join(", ")
                        ),
                ));
            }
        };
        Ok(Self { inner: params })
    }
}

impl<'de> Deserialize<'de> for Params {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Self::new(Vec::deserialize(deserializer)?).map_err(serde::de::Error::custom)
    }
}
