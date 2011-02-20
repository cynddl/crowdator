all: main.ml
	ocamlbuild -libs graphics,unix main.native

rtreetest:
	ocamlbuild -libs graphics,unix rtreetest.native

.PHONY: clean

clean:
	ocamlbuild -clean