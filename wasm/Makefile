.PHONY: serve
serve: build
	httplz

.PHONY: build
build:
	wasm-pack build --target web --dev -- --features=console_error_panic_hook

.PHONY: publish
publish: build
	local/publish.sh
