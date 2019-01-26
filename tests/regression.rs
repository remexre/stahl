static MODULE_SRC: &str = r#"
module main

; Got error "The target of a call must be a function, not #VAR:83#" in 149cd3b
def id-test-1
  (id _ S) Z

; Got panic "x not found" in 149cd3b
def const-test-1
  (const _ _ 1) 2
"#;

#[test]
fn regression_tests() {}
