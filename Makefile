all: main.ml
	ocamlbuild -libs graphics,unix main.native

rtreetest:
	ocamlbuild -libs graphics,unix rtreetest.native


opt-old:
	ocamlopt  -o main.out -p -cclib -lunix graphics.cmxa unix.cmxa primitives.ml matrix.ml matrixHopfield.ml mesh.ml hopfield.ml evolution.ml event.ml mbr.ml rtree.ml  dataset.ml graphics2D.ml main.ml

prof:
	ocamlbuild -libs graphics,unix -ocamlopt "ocamlopt -p" -no-links main.native
	cp _build/main.native .

.PHONY: clean

clean:
	rm -f *.cm[iox] *.o *.native *.out
	rm -f gmon.out
	ocamlbuild -clean
