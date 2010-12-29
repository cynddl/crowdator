all: main.ml
	ocamlbuild -libs graphics,unix main.native

.PHONY: clean

clean:
	ocamlbuild -clean