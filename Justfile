all: check build test

build:
	cargo build --all
build-release:
	cargo build --all --release
check:
	cargo check --all
clippy:
	cargo clippy --all
doc:
	cargo doc --all
run +ARGS="":
	cargo run -- -I . -I examples {{ARGS}}
test:
	cargo test --all -- --nocapture
watch +TARGETS="all":
	watchexec -cre lalrpop,rs,stahl,stahld -- just {{TARGETS}}

open-docs: doc
	cargo doc --open --package stahl
twelf:
	cd metatheory && watchexec -cre elf "echo make | twelf-server"
twelf-repl:
	cd metatheory && rlwrap -P make twelf-server
