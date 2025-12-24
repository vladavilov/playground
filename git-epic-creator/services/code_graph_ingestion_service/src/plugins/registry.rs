use crate::plugins::base::{IngestionContext, LanguagePlugin};
use serde_json::Value;

pub fn select_plugin<'a>(ctx: &IngestionContext, plugins: &'a [Box<dyn LanguagePlugin>]) -> anyhow::Result<&'a dyn LanguagePlugin> {
    for p in plugins {
        if p.name() == ctx.source_language.as_str() {
            return Ok(p.as_ref());
        }
    }
    let supported: Vec<String> = plugins.iter().map(|p| p.name().to_string()).collect();
    anyhow::bail!(
        "Unsupported source_language={:?}. Supported={:?}",
        ctx.source_language,
        supported
    );
}

pub fn run_selected_plugin(
    ctx: &IngestionContext,
    plugins: &[Box<dyn LanguagePlugin>],
) -> anyhow::Result<(Vec<crate::core::records::CodeNodeRecord>, Vec<crate::core::records::EdgeRecord>, Value)> {
    let plugin = select_plugin(ctx, plugins)?;
    let mut files = plugin.iter_files(ctx)?;
    files.sort_by_key(|p| p.to_string_lossy().replace('\\', "/"));
    let (nodes, edges, facts) = plugin.ingest(ctx, &files)?;

    let mut obj = serde_json::Map::new();
    obj.insert(plugin.name().to_string(), facts);
    Ok((nodes, edges, Value::Object(obj)))
}


