use stahl_context::Context;

/// An interpreter for Stahl.
#[derive(Debug)]
pub struct Interpreter<'a> {
    context: &'a mut Context,
}

impl<'a> Interpreter<'a> {
    /// Creates an interpreter that wraps a context.
    pub fn new(context: &'a mut Context) -> Interpreter<'a> {
        Interpreter { context }
    }
}
