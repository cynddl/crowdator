all: main.ml
	ocamlbuild -libs graphics,unix main.native

rtreetest:
	ocamlbuild -libs graphics,unix rtreetest.native


opt-old:
	ocamlopt  -o main.out -p -cclib -lunix graphics.cmxa unix.cmxa primitives.ml matrix.ml matrixHopfield.ml mesh.ml hopfield.ml evolution.ml event.ml mbr.ml rtree.ml  dataset.ml graphics2D.ml main.ml

prof:
	ocamlbuild -libs graphics,unix -ocamlopt "ocamlopt -p" -no-links main.native
	cp _build/main.native .

# Cross-compilation via mingw32. Fonctionne sous arch (avec 'minw32-ocaml' sur AUR).
# Devrait également fonctionner sous debian/ubuntu (changer le préfixe 'i486-mingw32-' par 'i586-mingw32msvc-' et installer 'minw32-ocaml')
win32:
	ocamlbuild -libs graphics,unix -no-links -ocamlc i486-mingw32-ocamlc -ocamlopt i486-mingw32-ocamlopt main.native
	cp _build/main.native crowdator.exe

.PHONY: clean

clean:
	rm -f *.cm[iox] *.o *.native *.out *.exe
	ocamlbuild -clean
