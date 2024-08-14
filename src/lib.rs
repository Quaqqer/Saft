#![allow(clippy::new_without_default)]
#![feature(map_try_insert, assert_matches)]

pub mod build;
pub mod cli;
pub mod lang;
pub mod span;

#[cfg(test)]
mod tests;
