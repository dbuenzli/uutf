#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let cmdliner = Env.bool "cmdliner"

let () =
  Pkg.describe "uutf" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/uutf";
    Pkg.bin ~cond:cmdliner ~auto:true "test/utftrip";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "test/examples.ml"; ]
