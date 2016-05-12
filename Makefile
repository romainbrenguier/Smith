
smith: main.ml
	ocamlbuild -use-menhir main.native
	mv main.native smith

