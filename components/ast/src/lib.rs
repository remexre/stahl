use stahl_parser::Value;

/// A top-level declaration.
#[derive(Debug)]
pub enum Decl {
    /// A constant.
    Def {},

    /// A (possibly recursive) function.
    Defn {},

    /// A type declaration.
    Type {},
}

/// An expression.
#[derive(Debug)]
pub enum Expr {
    /// A constant.
    Const(Value),
}
