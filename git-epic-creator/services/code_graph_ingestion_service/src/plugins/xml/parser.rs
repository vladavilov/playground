use regex::Regex;

#[derive(Debug, Clone)]
pub struct XmlWiring {
    pub class_names: Vec<String>,
}

fn re_class_attr() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r#"(?i)\bclass\s*=\s*"([^"]+)""#).unwrap())
}

pub fn extract_class_attributes(xml_text: &str) -> XmlWiring {
    let mut out: Vec<String> = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for cap in re_class_attr().captures_iter(xml_text) {
        let name = cap.get(1).map(|m| m.as_str().trim()).unwrap_or("");
        if name.is_empty() {
            continue;
        }
        if seen.insert(name.to_string()) {
            out.push(name.to_string());
        }
    }
    XmlWiring { class_names: out }
}


