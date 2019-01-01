use stahl_context::Context;
use stahl_modules::Module;
use stahl_util::SharedString;

/// Creates the `#compiler-intrinsics#` library.
pub fn create_compiler_intrinsics_lib(ctx: &mut Context) {
    let lib_name = SharedString::from("#compiler-intrinsics#");
    let mod_name = SharedString::from("");

    let mut lib_ctx = ctx.create_lib(lib_name.clone(), 0, 0, 0);
    lib_ctx
        .insert_mod(Module::intrinsics(lib_name, mod_name))
        .unwrap();
    lib_ctx.finish().unwrap();
}
