use std::io;

fn main() -> Result<(), serde_path_to_error::Error<serde_json::Error>> {
    let it: openrpc_types::OpenRPC =
        serde_path_to_error::deserialize(&mut serde_json::Deserializer::from_reader(io::stdin()))?;
    serde_path_to_error::serialize(&it, &mut serde_json::Serializer::pretty(io::stdout()))
}
