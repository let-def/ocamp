all:
	ocamlbuild -use-ocamlfind ocamp.native
	ln -sf ocamp.native ocamp

clean:
	ocamlbuild -use-ocamlfind -clean
