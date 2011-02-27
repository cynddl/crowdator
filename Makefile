all: main.ml
	ocamlbuild -libs graphics,unix main.native

rtreetest:
	ocamlbuild -libs graphics,unix rtreetest.native


opt:
	ocamlopt  -o main.out -p -cclib -lunix graphics.cmxa unix.cmxa primitives.ml matrix.ml matrixHopfield.ml mesh.ml hopfield.ml evolution.ml event.ml mbr.ml rtree.ml  dataset.ml graphics2D.ml main.ml
    

.PHONY: clean

clean:
	ocamlbuild -clean