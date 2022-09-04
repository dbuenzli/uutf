open B0_kit.V000

(* OCaml library names *)

let uutf = B0_ocaml.libname "uutf"
let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Libraries *)

let uutf_lib =
  let srcs = Fpath.[`Dir (v "src")] in
  let requires = [] in
  B0_ocaml.lib uutf ~doc:"The uutf library" ~srcs ~requires

(* Tests *)

let test =
  let srcs = Fpath.[`File (v "test/test.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ uutf ] in
  B0_ocaml.exe "test" ~doc:"Test suite" ~srcs ~meta ~requires

let utftrip =
  let doc = "Recode UTF-{8,16,16LE,16BE} and latin1 from stdin to stdout" in
  let srcs = Fpath.[`File (v "test/utftrip.ml")] in
  let requires = [unix; uutf; cmdliner] in
  B0_ocaml.exe "utftrip" ~public:true ~doc ~srcs ~requires

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The uutf programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/uutf"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/uutf/doc/"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/uutf.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/uutf/issues"
    |> B0_meta.(add description_tags)
      ["unicode"; "text"; "utf-8"; "utf-16"; "codec"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.depopts ["cmdliner", ""]
    |> B0_meta.add B0_opam.conflicts
      [ "cmdliner", {|< "1.1.0"|}]
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.03.0"|};
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
