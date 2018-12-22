all: check build test build-release

build:
	cargo build --all
build-release:
	cargo build --all --release
check:
	cargo check --all
run +ARGS="":
	cargo run -- {{ARGS}}
test:
	cargo test --all
watch TARGET="all":
	watchexec -cre lalrpop,rs -- just "{{TARGET}}"

twelf:
	cd metatheory && watchexec -cre elf "echo make | twelf-server"
twelf-repl:
	cd metatheory && rlwrap -P make twelf-server
