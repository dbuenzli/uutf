(* Examples from the documentation, this code is in public domain. *)

let recode ?nl ?encoding src out_encoding dst = 
  let i = Ucutf.make_input ?nl ?encoding src in 
  let o = Ucutf.make_output out_encoding dst in
  let rec copy i o = match Ucutf.input i with 
    | `Uchar u -> Ucutf.output o u; copy i o 
    | `Malformed -> Ucutf.output o Ucutf.u_rep; copy i o 
    | `Eoi -> ()
  in
  if out_encoding = `UTF_16 then Ucutf.output o Ucutf.u_bom;
  copy i o

let input_lines ?encoding src =
  let u_ps = 0x2029 (* PS, paragraph separator *) in 
  let u_ff = 0x000C (* FF, form feed *) in 
  let i = Ucutf.make_input ?encoding ~nl:u_ps src in
  let b = Buffer.create 512 in
  let rec aux i b acc = match Ucutf.input i with 
  | `Uchar u when u <> u_ps && u <> u_ff -> 
      Ucutf.add_utf8 b u; aux i b acc
  | `Uchar _ (* PS or FF *) -> 
      let s = Buffer.contents b in
      Buffer.clear b; aux i b (s :: acc)
  | `Malformed -> Ucutf.add_utf8 b Ucutf.u_rep; aux i b acc
  | `Eoi -> List.rev (Buffer.contents b :: acc)
  in
  aux i b []
