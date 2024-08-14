use std::{collections::HashMap, path::PathBuf};

use crate::lang::ast;
use crate::lang::parser;
use crate::span::Spanned;

pub struct SaftBuilder {
    root: PathBuf,
    source_cache: HashMap<ModuleName, String>,
}

/// A name for a module, really a path with the extension omitted.
///
/// Example: [std, asserts]
///   File hierarchy:
///     - root
///       - std
///         - asserts.saft
///             
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct ModuleName(Vec<String>);

impl ModuleName {
    pub fn parts(&self) -> &[String] {
        &self.0[..]
    }
}

type QueryResult<T> = Result<T, QueryError>;

#[derive(Debug)]
pub enum QueryError {
    MissingModule(ModuleName),
    ParseError(ModuleName, String),
}

impl SaftBuilder {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            source_cache: HashMap::new(),
        }
    }
    pub fn query_source(&mut self, module: &ModuleName) -> QueryResult<String> {
        if let Some(s) = self.source_cache.get(module) {
            return Ok(s.clone());
        }
        let mut path = self.root.clone();

        let parts = module.parts();

        for (i, name) in parts.iter().enumerate() {
            if i == parts.len() - 1 {
                path.push(format!("{}.saft", name));
            } else {
                path.push(name);
            }
        }

        println!("PATH: {:?}", path);

        match std::fs::read_to_string(path) {
            Ok(s) => {
                self.source_cache.insert(module.clone(), s.clone());
                Ok(s)
            }
            Err(_) => Err(QueryError::MissingModule(module.clone())),
        }
    }

    pub fn query_ast(&mut self, module: &ModuleName) -> QueryResult<Spanned<ast::Module>> {
        let source = self.query_source(module)?;
        match parser::SpannedModuleParser::new().parse(&source) {
            Ok(ast) => Ok(ast),
            Err(err) => Err(QueryError::ParseError(module.clone(), err.to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    #[test]
    fn sources() {
        let path = PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/res/query_test"));
        let mut sb = SaftBuilder::new(path);

        let main = &ModuleName(vec!["main".into()]);
        let lib_asserts = &ModuleName(vec!["lib".into(), "asserts".into()]);

        assert_eq!(
            sb.query_source(main).unwrap(),
            include_str!("../../res/query_test/main.saft")
        );

        assert_eq!(
            sb.query_source(lib_asserts).unwrap(),
            include_str!("../../res/query_test/lib/asserts.saft")
        );
    }

    #[test]
    fn ast() {
        let path = PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/res/query_test"));
        let mut sb = SaftBuilder::new(path);

        let main = &ModuleName(vec!["main".into()]);
        let lib_asserts = &ModuleName(vec!["lib".into(), "asserts".into()]);

        let _main = sb.query_ast(main).unwrap();
        let _lib_asserts = sb.query_ast(lib_asserts).unwrap();
    }
}
