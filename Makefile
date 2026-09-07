.PHONY: build
build:
	dune build

.PHONY: test
test:
	ln -sf _build/default/tests/Test_gencache.exe test
	dune exec ./tests/Test_gencache.exe

.PHONY: setup
setup:
	opam install --deps-only --with-test --with-doc .
