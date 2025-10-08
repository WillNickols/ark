/*
 * wire_message.rs
 *
 * Copyright (C) 2022 Posit Software, PBC. All rights reserved.
 *
 */

use generic_array::GenericArray;
use hmac::Hmac;
use log::trace;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde::Serialize;
use serde_json::json;
use serde_json::value::Value;
use sha2::Sha256;

use crate::error::Error;
// use crate::socket::socket::Socket; // ZMQ-based, no longer used
use crate::wire::header::JupyterHeader;
use crate::wire::jupyter_message::JupyterMessage;
use crate::wire::jupyter_message::ProtocolMessage;


/// Represents an untyped Jupyter message delivered over the wire. A WireMessage
/// can represent any kind of Jupyter message; typically its header will be
/// examined and it will be converted into a typed JupyterMessage.
#[derive(Debug, Serialize, Deserialize)]
pub struct WireMessage {
    /// The header for this message
    pub header: JupyterHeader,

    /// The header of the message from which this message originated, if any.
    /// If none, it's serialized as an empty dict as required by the Jupyter
    /// protocol.
    #[serde(
        serialize_with = "serialize_none_as_empty_dict",
        deserialize_with = "deserialize_empty_dict_as_none"
    )]
    pub parent_header: Option<JupyterHeader>,

    /// Additional metadata, if any
    pub metadata: Value,

    /// The body (payload) of the message
    pub content: Value,
}

impl WireMessage {
    /// Return the Jupyter type of the message.
    pub fn message_type(&self) -> String {
        self.header.msg_type.clone()
    }

    fn msg_type(&self) -> String {
        match self.header.msg_type.as_str() {
            "comm_msg" => {
                if let Value::Object(map) = &self.content {
                    let comm_id = Self::comm_msg_id(map.get("comm_id"));
                    let comm_msg_type = Self::comm_msg_type(map.get("data"));
                    return format!("comm_msg/{comm_id}/{comm_msg_type}");
                }
            },
            "status" => {
                if let Value::Object(map) = &self.content {
                    if let Some(Value::String(execution_state)) = map.get("execution_state") {
                        return format!("status/{execution_state}");
                    }
                }
            },
            _ => {},
        }
        self.header.msg_type.clone()
    }

    fn comm_msg_type(data: Option<&Value>) -> String {
        if let Some(Value::Object(map)) = data {
            if let Some(Value::String(msg_type)) = map.get("method") {
                return msg_type.clone();
            }
        }
        String::from("unknown")
    }

    fn comm_msg_id(id: Option<&Value>) -> String {
        if let Some(Value::String(id)) = id {
            return Self::comm_msg_id_type(&id);
        }
        String::from("unknown")
    }

    fn comm_msg_id_type(id: &str) -> String {
        if id.contains("frontEnd-") {
            return String::from("frontEnd");
        }
        if id.contains("variables-") {
            return String::from("variables");
        }
        if id.contains("dataViewer-") {
            return String::from("dataViewer");
        }
        if id.contains("help-") {
            return String::from("help");
        }
        if id.contains("lsp-") {
            return String::from("LSP");
        }
        if id.contains("dap-") {
            return String::from("DAP");
        }
        return id.to_string();
    }
}

// Conversion: WireMessage (untyped) -> JupyterMessage (typed); used on
// messages we receive over the wire to parse into the correct type.
impl<T: ProtocolMessage + DeserializeOwned> TryFrom<&WireMessage> for JupyterMessage<T> {
    type Error = crate::error::Error;
    fn try_from(msg: &WireMessage) -> Result<JupyterMessage<T>, Error> {
        let content = match serde_json::from_value(msg.content.clone()) {
            Ok(val) => val,
            Err(err) => {
                return Err(Error::InvalidMessage(
                    T::message_type(),
                    msg.content.clone(),
                    err,
                ))
            },
        };
        Ok(JupyterMessage {
            header: msg.header.clone(),
            parent_header: msg.parent_header.clone(),
            content,
        })
    }
}

// Conversion: JupyterMessage (typed) -> WireMessage (untyped); used prior to
// sending messages to get them ready for dispatch.
impl<T: ProtocolMessage> TryFrom<&JupyterMessage<T>> for WireMessage {
    type Error = crate::error::Error;

    /// Convert a typed JupyterMessage into a WireMessage, preserving ZeroMQ
    /// socket identities.
    fn try_from(msg: &JupyterMessage<T>) -> Result<Self, Error>
    where
        T: ProtocolMessage,
    {
        let content = match serde_json::to_value(msg.content.clone()) {
            Ok(val) => val,
            Err(err) => return Err(Error::CannotSerialize(err)),
        };
        Ok(Self {
            header: msg.header.clone(),
            parent_header: msg.parent_header.clone(),
            metadata: json!({}),
            content,
        })
    }
}

// Currently unused but better be safe
fn serialize_none_as_empty_dict<S, T>(option: &Option<T>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: serde::Serialize,
{
    match option {
        Some(value) => value.serialize(serializer),
        None => serde_json::Map::new().serialize(serializer),
    }
}

fn deserialize_empty_dict_as_none<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: serde::de::DeserializeOwned,
{
    let value: Value = serde::Deserialize::deserialize(deserializer)?;
    
    // If it's an empty object or null, treat as None
    match value {
        Value::Object(map) if map.is_empty() => Ok(None),
        Value::Null => Ok(None),
        _ => {
            // Try to deserialize as T
            let result: T = serde_json::from_value(value)
                .map_err(serde::de::Error::custom)?;
            Ok(Some(result))
        }
    }
}
