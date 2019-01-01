use stahl_context::Context;
use stahl_modules::Module;
use stahl_util::SharedString;

/// Creates the `#compiler-intrinsics#` library.
pub fn create_compiler_intrinsics_lib(ctx: &mut Context) {
    let lib_name = SharedString::from("#compiler-intrinsics#");
    let mod_name = SharedString::from("");

    ctx.create_lib(lib_name.clone(), 0, 0, 0)
        .unwrap()
        .insert_mod(mod_name.clone(), Module::intrinsics(lib_name, mod_name))
        .unwrap();
}
