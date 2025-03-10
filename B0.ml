open B0_kit.V000

(* OCaml library names *)

let uutf = B0_ocaml.libname "uutf"
let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Libraries *)

let uutf_lib = B0_ocaml.lib uutf ~srcs:[`Dir ~/"src"]

(* Tests *)

let test_uutf = B0_ocaml.test ~/"test/test_uutf.ml" ~requires:[uutf]

let utftrip =
  let srcs = [`File ~/"test/utftrip.ml"] in
  let requires = [unix; uutf; cmdliner] in
  B0_ocaml.exe "utftrip" ~public:true ~srcs ~requires

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The uutf programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/uutf"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/uutf/doc/"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/uutf.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/uutf/issues"
    |> ~~ B0_meta.description_tags
      ["unicode"; "text"; "utf-8"; "utf-16"; "codec"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> ~~ B0_opam.depopts ["cmdliner", ""]
    |> ~~ B0_opam.conflicts
      [ "cmdliner", {|< "1.3.0"|}]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-cmdliner" "%{cmdliner:installed}%"]]|}
  in
  B0_pack.make "default" ~doc:"uutf package" ~meta ~locked:true @@
  B0_unit.list ()
