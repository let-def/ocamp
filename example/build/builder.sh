#!/bin/sh

### Configuration
PACKAGES=lwt.unix,cmdliner

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt

SOURCES="utils.ml heart.mli heart.ml common.ml client.ml server.ml ocamp.ml"
MAIN=ocamp

### RULES
SELF="$0"

packages() {
  echo "$PACKAGES"
}

flags() {
  OCAMLFIND_COMMANDS='ocamlc=echo,ocamlopt=echo' ocamlfind "$@"
}

ocamlc_c_flags() {
  flags c -package `packages`
}

ocamlc_ld_flags() {
  flags c -package `packages` -linkpkg
}

ocamlopt_c_flags() {
  flags opt -package `packages`
}

ocamlopt_ld_flags() {
  flags opt -package `packages` -linkpkg
}

builder() {
  while [[ "$1" == *"="* ]]; do
    eval export "$1"
    echo >&2 "EXPORT $1"
    shift 1
  done
  exec $ocamp hipp sh "$SELF" "$@"
}

stir_file() {
  $ocamp stir md5sum "$1"
}

cmi() {
  TARGET="$1"
  TARGET="${TARGET%.*}"
  echo >&2 CMI "'$TARGET'"
  if [ -f "${TARGET}.mli" ]; then
    $OCAMLC `builder ocamlc_c_flags` -c "${TARGET}.mli"
    echo "CMI"
  elif [ -f "${TARGET}.ml" ]; then
    echo >&2 PREFER $PREFER
    if [ "x$PREFER" = "xCMX" ]; then
      $OCAMLOPT `builder ocamlopt_c_flags` -c "${TARGET}.ml"
      echo >&2 CMX "'$TARGET'"
      echo "CMX"
    else
      $OCAMLC `builder ocamlc_c_flags` -c "${TARGET}.ml"
      echo >&2 CMO "'$TARGET'"
      echo "CMO"
    fi
  else
    exit 1
  fi
}

cmo() {
  TARGET="$1"
  TARGET=${TARGET%.*}
  echo >&2 CMO "'$TARGET'"
  CMO_OR_CMX=`builder PREFER=CMO cmi "${TARGET}"`
  if [ "$CMO_OR_CMX" != "CMO" ]; then
    $OCAMLC `builder ocamlc_c_flags` -c "${TARGET}.ml"
  fi
}

cmx() {
  TARGET="$1"
  TARGET=${TARGET%.*}
  echo >&2 CMX "'$TARGET'"
  CMO_OR_CMX=`builder PREFER=CMX cmi "${TARGET}"`
  if [ "$CMO_OR_CMX" != "CMX" ]; then
    $OCAMLOPT `builder ocamlopt_c_flags` -c "${TARGET}.ml"
  fi
}

depend() {
  if [ -z "$1" ]; then
    ocamldep $SOURCES
  else
    builder depend | grep "^$1" | cut -d: -f2
  fi
}

build() {
  TARGET="$1"
  if [ -z "$TARGET" ]; then
    TARGET=${MAIN}.cmx
  fi
  DEPS=`builder depend "$TARGET"`
  for DEP in $DEPS; do
    builder build "$DEP" >/dev/null &
  done
  for DEP in $DEPS; do
    ( builder build "$DEP" )
  done
  if [[ "$TARGET" == *".cmi" ]]; then
    builder cmi "$TARGET" >/dev/null
  elif [[ "$TARGET" == *".cmo" ]]; then
    builder cmo "$TARGET" >/dev/null
  elif [[ "$TARGET" == *".cmx" ]]; then
    builder cmx "$TARGET" >/dev/null
  fi
}

echo >&2 "EVAL '$@'"
eval "$@"
