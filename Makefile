all:
	ocamlbuild -use-ocamlfind hipp.native

clean:
	ocamlbuild -use-ocamlfind -clean
