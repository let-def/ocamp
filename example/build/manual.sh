#!/bin/sh

build_ml() {
  ocamlfind opt -package lwt.unix,cmdliner -c "$1"
}

link_ml() {
  OUT="$1"
  shift 1
  ocamlfind opt -package lwt.unix,cmdliner -linkpkg -o "$1" "$@"
}

build_ml utils.ml
build_ml heart.mli  
build_ml heart.ml
build_ml common.ml
build_ml client.ml
build_ml server.ml
build_ml ocamp.ml
link_ml ocamp utils.cmx heart.cmx common.cmx client.cmx server.cmx ocamp.cmx
