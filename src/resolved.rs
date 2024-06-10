// This file is @generated by library tests

//! Parallel types where [`ReferenceOr<T>`] is replaced by item `T`.

use crate::*;
use semver::Version;
use serde::{Deserialize, Serialize};

/// > This is the root object of the OpenRPC document.
/// > The contents of this object represent a whole OpenRPC document.
/// > How this object is constructed or stored is outside the scope of the OpenRPC Specification.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
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
    /// > An array of Server Objects,
    /// > which provide connectivity information to a target server.
    /// > If the servers property is not provided, or is an empty array,
    /// > the default value would be a Server Object with a url value of `localhost`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub servers: Option<Vec<Server>>,
    /// > REQUIRED.
    /// > The available methods for the API.
    /// > While it is required, the array may be empty (to handle security filtering, for example).
    pub methods: Vec<Method>,
    /// > An element to hold various schemas for the specification.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<Components>,
    /// > Additional external documentation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub external_docs: Option<ExternalDocumentation>,
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}
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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<Tag>>,
    /// > A short summary of what the method does.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    /// > A verbose explanation of the method behavior.
    /// > GitHub Flavored Markdown syntax MAY be used for rich text representation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// > Additional external documentation for this method.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub external_docs: Option<ExternalDocumentation>,
    /// > REQUIRED.
    /// > A list of parameters that are applicable for this method.
    /// > The list MUST NOT include duplicated parameters and therefore require name to be unique.
    /// > The list can use the Reference Object to link to parameters that are defined by the Content Descriptor Object.
    /// > All optional params (content descriptor objects with “required”: false) MUST be positioned after all required params in the list.
    pub params: Vec<ContentDescriptor>,
    /// > The description of the result returned by the method.
    /// > If defined, it MUST be a Content Descriptor or Reference Object.
    /// > If undefined, the method MUST only be used as a notification.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<ContentDescriptor>,
    /// > Declares this method to be deprecated.
    /// > Consumers SHOULD refrain from usage of the declared method.
    /// > Default value is `false`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub deprecated: Option<bool>,
    /// > An alternative servers array to service this method.
    /// > If an alternative servers array is specified at the Root level,
    /// > it will be overridden by this value.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub servers: Option<Vec<Server>>,
    /// > A list of custom application defined errors that MAY be returned.
    /// > The Errors MUST have unique error codes.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub errors: Option<Vec<Error>>,
    // /// > A list of possible links from this method call.
    // pub links: Option<Vec<Link>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub param_structure: Option<ParamStructure>,
    /// > Array of Example Pairing Objects where each example includes a valid params-to-result Content Descriptor pairing.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub examples: Option<Vec<ExamplePairing>>,
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}
/// > The Example Pairing object consists of a set of example params and result.
/// > The result is what you can expect from the JSON-RPC service given the exact params.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct ExamplePairing {
    /// > REQUIRED Name for the example pairing.
    pub name: String,
    /// > A verbose explanation of the example pairing.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// > Short description for the example pairing.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    /// > REQUIRED Example parameters.
    pub params: Vec<Example>,
    /// > Example result.
    /// > When undefined, the example pairing represents usage of the method as a notification.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Example>,
    #[serde(flatten)]
    pub extensions: SpecificationExtensions,
}
