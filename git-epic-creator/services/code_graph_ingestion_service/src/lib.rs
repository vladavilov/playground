// `libgit2-sys` (via `git2`) doesn't always pull in advapi32 on MSVC.
// We depend on `git2` for deterministic git materialization, so ensure the link is present.
#[cfg(windows)]
#[link(name = "advapi32")]
unsafe extern "system" {}

pub mod core;
pub mod plugins;
pub mod persistence;
pub mod web;


