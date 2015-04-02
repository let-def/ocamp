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
    if [ "x$PREFER" = "xCMX" ]; then
      $OCAMLOPT `builder ocamlopt_c_flags` -c "${TARGET}.ml"
      echo >&2 CMX_CMI "'$TARGET'"
      echo "CMX"
    else
      $OCAMLC `builder ocamlc_c_flags` -c "${TARGET}.ml"
      echo >&2 CMO_CMI "'$TARGET'"
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

byte() {
  TARGET="$1"
  TARGET=${TARGET%.*}
  (builder build "${TARGET}.cmo")
  echo >&2 BYTE "'$TARGET'"
  $OCAMLC `builder ocamlc_ld_flags` -o "${TARGET}.byte" `link_depend cmo`
}

native() {
  TARGET="$1"
  TARGET=${TARGET%.*}
  (builder build "${TARGET}.cmx")
  echo >&2 NATIVE "'$TARGET'"
  $OCAMLOPT `builder ocamlopt_ld_flags` -o "${TARGET}.native" `link_depend cmx`
}

depend() {
  if [ -z "$1" ]; then
    ocamldep $SOURCES
  else
    builder depend | grep "^$1" | cut -d: -f2
  fi
}

link_depend() {
  # $1 = cmx or cmo
  builder depend | while read name colon deps; do
    for dep in $deps; do
      echo "$dep $name";
    done;
  done | tsort | grep '.$1\$'
}

build() {
  echo >&2 BUILD "$@"
  TARGET="$1"
  if [ -z "$TARGET" ]; then
    TARGET=${MAIN}.byte
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
  elif [[ "$TARGET" == *".byte" ]]; then
    builder byte "$TARGET" >/dev/null
  elif [[ "$TARGET" == *".native" ]]; then
    builder native "$TARGET" >/dev/null
  fi
}

eval "$@"
