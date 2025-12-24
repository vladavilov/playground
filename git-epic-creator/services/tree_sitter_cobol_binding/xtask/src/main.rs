use std::process::{Command, Stdio};

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let cmd = args.next().unwrap_or_else(|| "help".to_string());

    match cmd.as_str() {
        "regen" => regen(),
        "help" | "-h" | "--help" => {
            eprintln!(
                r#"xtask (tree_sitter_cobol_binding)

Commands:
  regen   Regenerate vendor/tree-sitter-cobol/src/parser.c (+scanner.c) using npx tree-sitter-cli@0.20.7

Notes:
  - Developer-only: requires Node.js (npx).
  - Runtime/service builds do NOT run regen; they compile the committed C sources.
"#
            );
            Ok(())
        }
        other => anyhow::bail!("unknown command: {other} (try `cargo run -p xtask -- help`)"),
    }
}

fn regen() -> anyhow::Result<()> {
    let repo_root = std::env::current_dir()?;
    let vendor_dir = repo_root
        .join("vendor")
        .join("tree-sitter-cobol");

    // Equivalent to the previous scripts/regen_parser_sources.*:
    // - run `tree-sitter generate` (pinned)
    // - delete extra files to keep the vendor capsule lean
    run(
        Command::new("npx")
            .arg("tree-sitter-cli@0.20.7")
            .arg("generate")
            .current_dir(&vendor_dir),
    )?;

    // Keep only what we need for deterministic builds.
    remove_if_exists(vendor_dir.join("Cargo.toml"))?;
    remove_if_exists(vendor_dir.join("binding.gyp"))?;
    remove_dir_if_exists(vendor_dir.join("bindings"))?;
    remove_if_exists(vendor_dir.join("src").join("grammar.json"))?;
    remove_if_exists(vendor_dir.join("src").join("node-types.json"))?;

    Ok(())
}

fn run(cmd: &mut Command) -> anyhow::Result<()> {
    cmd.stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());
    let status = cmd.status()?;
    if !status.success() {
        anyhow::bail!("command failed: {status}");
    }
    Ok(())
}

fn remove_if_exists(path: std::path::PathBuf) -> anyhow::Result<()> {
    match std::fs::remove_file(&path) {
        Ok(_) => Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e.into()),
    }
}

fn remove_dir_if_exists(path: std::path::PathBuf) -> anyhow::Result<()> {
    match std::fs::remove_dir_all(&path) {
        Ok(_) => Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e.into()),
    }
}

