use maplit::hashmap;
use stahl_ast::LibName;
use stahl_context::Context;
use stahl_modules::Module;
use stahl_util::SharedString;

/// Creates the `#compiler-builtins#` library.
pub fn create_compiler_builtins_lib(ctx: &mut Context) {
    let lib_name = LibName("#compiler-builtins#".into(), 0, 0, 0);
    let mod_name = SharedString::from("");

    let mut lib_ctx = ctx.create_lib(lib_name.clone(), hashmap! {});
    lib_ctx
        .insert_mod(Module::builtins(lib_name, mod_name))
        .unwrap();
    lib_ctx.finish().unwrap();
}
