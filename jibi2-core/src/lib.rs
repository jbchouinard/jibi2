#![feature(maybe_uninit_slice)]
#![allow(clippy::new_without_default)]
pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod error;
pub mod instruction;
pub mod native;
pub mod object;
pub mod reader;
pub mod stack;
pub mod vm;
