use std::{fmt, str::FromStr};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum CodeRelType {
    Calls,
    Performs,
    References,
    Imports,
    Includes,
    Reads,
    Writes,
    ConfigWires,
    Contains,
    NextChunk,
}

impl CodeRelType {
    pub const fn as_str(self) -> &'static str {
        match self {
            CodeRelType::Calls => "CALLS",
            CodeRelType::Performs => "PERFORMS",
            CodeRelType::References => "REFERENCES",
            CodeRelType::Imports => "IMPORTS",
            CodeRelType::Includes => "INCLUDES",
            CodeRelType::Reads => "READS",
            CodeRelType::Writes => "WRITES",
            CodeRelType::ConfigWires => "CONFIG_WIRES",
            CodeRelType::Contains => "CONTAINS",
            CodeRelType::NextChunk => "NEXT_CHUNK",
        }
    }

}

impl fmt::Display for CodeRelType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for CodeRelType {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let t = s.trim().to_ascii_uppercase();
        match t.as_str() {
            "CALLS" => Ok(CodeRelType::Calls),
            "PERFORMS" => Ok(CodeRelType::Performs),
            "REFERENCES" => Ok(CodeRelType::References),
            "IMPORTS" => Ok(CodeRelType::Imports),
            "INCLUDES" => Ok(CodeRelType::Includes),
            "READS" => Ok(CodeRelType::Reads),
            "WRITES" => Ok(CodeRelType::Writes),
            "CONFIG_WIRES" => Ok(CodeRelType::ConfigWires),
            "CONTAINS" => Ok(CodeRelType::Contains),
            "NEXT_CHUNK" => Ok(CodeRelType::NextChunk),
            _ => Err("unsupported rel_type"),
        }
    }
}


