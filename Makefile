.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune exec ./tests/Test_gencache.exe

.PHONY: setup
setup:
	opam install --deps-only --with-test --with-doc .
