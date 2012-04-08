(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* UTF decoders. *)

let utf8_len = [|         (* uchar byte length according to first UTF-8 byte. *)
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 
  0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 
  2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 
  4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
    
let utf8_decode r =
  try
    let b0 = r () in
    try match utf8_len.(b0) with
    | 0 -> `Malformed
    | 1 -> `Uchar b0
    | 2 ->
	let b1 = r () in
	if b1 lsr 6 != 0b10 then `Malformed else
	`Uchar (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F))
    | 3 ->
	let b1 = r () in let b2 = r () in
	let c = `Uchar (((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor 
			(b2 land 0x3F))
	in
	if b2 lsr 6 != 0b10 then `Malformed else
	begin match b0 with
	| 0xE0 -> if b1 < 0xA0 || 0xBF < b1 then `Malformed else c
	| 0xED -> if b1 < 0x80 || 0x9F < b1 then `Malformed else c
	| _ -> if b1 lsr 6 != 0b10 then `Malformed else c
	end
    | 4 -> 
	let b1 = r () in let b2 = r () in let b3 = r () in
	let c = `Uchar (((b0 land 0x07) lsl 18) lor ((b1 land 0x3F) lsl 12) lor 
			((b2 land 0x3F) lsl 6) lor (b3 land 0x3F))
	in
	if b3 lsr 6 != 0b10 || b2 lsr 6 != 0b10 then `Malformed else
	begin match b0 with
	| 0xF0 -> if b1 < 0x90 || 0xBF < b1 then `Malformed else c
	| 0xF4 -> if b1 < 0x80 || 0x8F < b1 then `Malformed else c
	| _ -> if b1 lsr 6 != 0b10 then `Malformed else c
	end
    | _ -> assert false
    with End_of_file -> `Malformed
  with End_of_file -> `Eoi
      
let utf16be_decode r =         (* same as utf6le_decode but swap input bytes. *)
  try
    let b0 = r () in 
    try
      let b1 = r () in 
      let u0 = (b0 lsl 8) lor b1 in
      if u0 < 0xD800 || u0 > 0xDFFF then `Uchar u0 else
      if u0 > 0xDBFF then `Malformed else
      let b2 = r () in 
      let b3 = r () in 
      let u1 = (b2 lsl 8) lor b3 in
      if u1 < 0xDC00 || u1 > 0xDFFF then `Malformed else
      `Uchar ((((u0 land 0x3FF) lsl 10) lor (u1 land 0x3FF)) + 0x10000)
    with End_of_file -> `Malformed
  with End_of_file -> `Eoi
      
let utf16le_decode r =         (* same as utf6be_decode but swap input bytes. *)
  try
    let b0 = r () in 
    try
      let b1 = r () in 
      let u0 = (b1 lsl 8) lor b0 in
      if u0 < 0xD800 || u0 > 0xDFFF then `Uchar u0 else
      if u0 > 0xDBFF then `Malformed else
      let b2 = r () in 
      let b3 = r () in 
      let u1 = (b3 lsl 8) lor b2 in
      if u1 < 0xDC00 || u1 > 0xDFFF then `Malformed else
      `Uchar ((((u0 land 0x3FF) lsl 10) lor (u1 land 0x3FF)) + 0x10000)
    with End_of_file -> `Malformed
  with End_of_file -> `Eoi

(* UTF encoders. WARNING assume unicode scalar values as input. *)

let utf8_encode w u = 
  if u <= 0x007F then 
    (w u)
  else if u <= 0x07FF then 
    (w (0xC0 lor (u lsr 6)); 
     w (0x80 lor (u land 0x3F)))
  else if u <= 0xFFFF then
    (w (0xE0 lor (u lsr 12));
     w (0x80 lor ((u lsr 6) land 0x3F));
     w (0x80 lor (u land 0x3F)))
  else
    (w (0xF0 lor (u lsr 18));
     w (0x80 lor ((u lsr 12) land 0x3F));
     w (0x80 lor ((u lsr 6) land 0x3F));
     w (0x80 lor (u land 0x3F)))

let utf16be_encode w u =            (* same as utf16le_encode but swap bytes. *)
  if u < 0x10000 then (w (u lsr 8); w (u land 0xFF)) else
  let u' = u - 0x10000 in 
  let u0 = (0xD800 lor (u' lsr 10)) in
  let u1 = (0xDC00 lor (u' land 0x3FF)) in
  w (u0 lsr 8); w (u0 land 0xFF);
  w (u1 lsr 8); w (u1 land 0xFF)

let utf16le_encode w u =            (* same as utf16be_encode but swap bytes. *)
  if u < 0x10000 then (w (u land 0xFF); w (u lsr 8)) else
  let u' = u - 0x10000 in 
  let u0 = (0xD800 lor (u' lsr 10)) in
  let u1 = (0xDC00 lor (u' land 0x3FF)) in
  w (u0 land 0xFF); w (u0 lsr 8); 
  w (u1 land 0xFF); w (u1 lsr 8)

(* Unicode characters and encoding schemes *)

type encoding = [ `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]
type uchar = int

let u_bom = 0xFEFF                                                    (* BOM. *)
let u_rep = 0xFFFD                                  (* replacement character. *)
let u_lf  = 0x000A                                              (* line feed. *)
let u_cr  = 0x000D                                        (* carriage return. *)
let u_nel = 0x0085                                              (* next line. *)
let u_ls  = 0x2028                                         (* line separator. *)

let is_uchar u = 0x0000 <= u && u < 0xD800 && 0xDFFF < u && u <= 0x10FFFF
let print_cp ppf u =
  if u < 0 || u > 0x10FFFF then Format.fprintf ppf "U+Invalid(%X)" u else
  if u <= 0xFFFF then Format.fprintf ppf "U+%04X" u else
  Format.fprintf ppf "U+%X" u

let cp_to_string u =                                      (* NOT thread safe. *)
  Format.fprintf Format.str_formatter "%a" print_cp u;
  Format.flush_str_formatter ()
  
(* Input *)

type result = [ `Uchar of uchar | `Eoi | `Malformed ]
type pos = int * int
type source = [ 
  | `Channel of Pervasives.in_channel 
  | `Fun of unit -> int 
  | `String of int * string ]

let byte_reader_of_source = function 
  | `Channel ic -> fun () -> input_byte ic
  | `Fun f -> f
  | `String (pos, s) -> 
      let len = String.length s in 
      if pos < 0 || pos > len then 
	invalid_arg (Printf.sprintf "index out of bounds (%d)" pos) 
      else
	let pos = ref pos in 
	fun () -> 
	  if !pos >= len then raise End_of_file else
	  let u = Char.code (String.unsafe_get s !pos) in (incr pos; u)

type input = 
    { mutable r : unit -> int;                                (* byte reader. *)
      mutable decode : (unit -> int) -> result;         (* character decoder. *)
      input : input -> result;                             (* input function. *)
      mutable encoding : encoding;            (* byte stream encoding scheme. *)
      mutable removed_bom : bool;      (* true if BOM was parsed and removed. *)
      mutable peek : result;                          (* character lookahead. *)
      mutable last_cr : bool;                    (* true if last char was CR. *)
      nl : int;                                  (* newline replacement char. *)
      mutable line : int;                                     (* line number. *)
      mutable col : int;                                    (* column number. *)
      mutable count : int; }                                   (* char count. *)

let incr_line i = i.count <- i.count + 1; i.col <- 0; i.line <- i.line + 1
let incr_col i = i.count <- i.count + 1; i.col <- i.col + 1

let peek_raw i = i.decode i.r
let input_raw i = let c = i.peek in i.peek <- peek_raw i; c
let input_raw_pos i = match i.peek with
| `Uchar u as c when u = u_ls || u = u_nel -> 
    i.last_cr <- false; incr_line i; i.peek <- peek_raw i; c
| `Uchar u as c when u = u_cr -> 
    i.last_cr <- true; incr_line i; i.peek <- peek_raw i; c
| `Uchar u as c when u = u_lf -> 
    (if i.last_cr then (i.last_cr <- false; i.count <- i.count + 1) else 
     incr_line i);                                            (* CR LF mess. *)
     i.peek <- peek_raw i; c
| `Eoi as c -> c
| c -> i.last_cr <- false; incr_col i; i.peek <- peek_raw i; c

let peek_nnl i =                        (* peek with new line normalization. *)
  match
    let c = i.decode i.r in
    if not i.last_cr then c else                              (* CR LF mess. *)
    (i.last_cr <- false; if c = `Uchar u_lf then i.decode i.r else c)
  with 
  | `Uchar u when u = u_lf || u = u_ls || u = u_nel -> `Uchar i.nl
  | `Uchar u when u = u_cr -> (i.last_cr <- true; `Uchar i.nl)
  | c -> c
    
let input_nnl i = let c = i.peek in i.peek <- peek_nnl i; c
let input_nnl_pos i = match i.peek with
| `Uchar u as c when u = i.nl -> incr_line i; i.peek <- peek_nnl i; c
| `Eoi as c -> c
| c -> incr_col i; i.peek <- peek_nnl i; c

let init_input i encoding =  
  let r_opt () = try Some (i.r ()) with End_of_file -> None in
  let set ?(bom = false) ?(r = i.r) e = 
    i.removed_bom <- bom; i.r <- r;
    i.decode <- match e with 
    | `UTF_8 -> utf8_decode 
    | `UTF_16BE -> utf16be_decode 
    | `UTF_16LE -> utf16le_decode
    | _ -> assert false
  in  
  let push i bytes k =                                    (* push back bytes. *)
    let bytes = ref bytes in 
    fun () -> match !bytes with
    | [] -> i.r <- k; k ()  (* soon or later we won't go through the closure. *)
    | b :: r -> bytes := r; b
  in
  match encoding with
  | Some (`UTF_8 | `UTF_16BE | `UTF_16LE as e) -> set e
  | Some `UTF_16 ->                                   
      begin match r_opt () with                        (* try to parse a BOM. *)
      | None -> set `UTF_16BE                                  (* empty file. *)
      | Some b0 -> 
	  begin match r_opt () with
	  | None -> set ~r:(push i [b0] i.r) `UTF16_BE   (* 1/2 byte in file. *)
	  | Some b1 -> match b0, b1 with 
	    | 0xFE, 0xFF -> set ~bom:true `UTF_16BE          (* UTF-16BE BOM. *)
	    | 0xFF, 0xFE -> set ~bom:true `UTF_16LE          (* UTF-16LE BOM. *)
	    | b0, b1 ->                                (* no BOM => UTF-16BE. *)
		set ~r:(push i [b0; b1] i.r) `UTF_16BE;
	  end
      end
  | None ->                                         (* try to guess encoding. *)
      begin match r_opt () with
      | None -> set `UTF_8                                     (* empty file. *)
      | Some (0xFE as b0) -> 
	  begin match r_opt () with           (* try to parse a UTF-16BE BOM. *)
	  | None -> set ~r:(push i [b0] i.r) `UTF_16BE   (* 1/2 byte in file. *)
	  | Some 0xFF -> set ~bom:true `UTF_16BE             (* UTF-16BE BOM. *)
	  | Some b1 ->      (* no BOM => UTF16BE as UTF-8 would be malformed. *)
	      set ~r:(push i [b0; b1] i.r) `UTF_16BE
	  end
      | Some (0xFF as b0) ->                  (* try to parse a UTF-16LE BOM. *)
	  begin match r_opt () with
	  | None -> set ~r:(push i [b0] i.r) `UTF_16LE   (* 1/2 byte in file. *)
	  | Some 0xFE -> set ~bom:true `UTF_16LE             (* UTF-16LE BOM. *)
	  | Some b1 ->      (* no BOM => UTF16BE as UTF-8 would be malformed. *)
	      set ~r:(push i [b0; b1] i.r) `UTF_16BE
	  end
      | Some (0xEF as b0) ->                    (* try to parse an UTF-8 BOM. *)
	  begin match r_opt () with
	  | None -> set ~r:(push i [b0] i.r) `UTF_8      (* 1/3 byte in file. *)
	  | Some (0xBB as b1) ->
	      begin match r_opt () with 
	      | None ->                                 (* 2/3 bytes in file. *)
		  set ~r:(push i [b0; b1] i.r) `UTF_8   
	      | Some 0xBF -> set ~bom:true `UTF_8               (* UTF-8 BOM. *)
	      | Some b2 ->                           (* no BOM, assume UTF-8. *)
		  set ~r:(push i [b0; b1; b2] i.r) `UTF_8
	      end
	  | Some b1 ->                               (* no BOM, assume UTF-8. *)
	      set ~r:(push i [b0; b1] i.r) `UTF_8
	  end
      | Some b0 ->                                   (* no BOM, assume UTF-8. *)
	  set ~r:(push i [b0] i.r) `UTF_8
      end

let make_input ?(pos = false) ?nl ?encoding src =
  let r = byte_reader_of_source src in
  let nl, line, input, peek = match pos, nl with 
  | false, None -> (u_lf, 0, input_raw, peek_raw)
  | true, None -> (u_lf, 1, input_raw_pos, peek_raw)
  | false, Some nl -> (nl, 0, input_nnl, peek_nnl)
  | true, Some nl -> (nl, 1, input_nnl_pos, peek_nnl)
  in
  let i = 
    { r = r; decode = utf8_decode; input = input; encoding = `UTF_8; 
      removed_bom = false; peek = `Eoi; last_cr = false; nl = nl; line = line; 
      col = 0; count = 0; }
  in
  init_input i encoding;
  i.peek <- peek i;
  i

let input_encoding i = i.encoding
let pos i = i.line, i.col
let count i = i.count
let removed_bom i = i.removed_bom
let input i = i.input i
let peek i = i.peek

(* Output *)

type dest = 
    [ `Channel of out_channel | `Buffer of Buffer.t | `Fun of (int -> unit) ]

let byte_writer_of_dest = function
  | `Channel c -> output_byte c
  | `Buffer b -> fun byte -> Buffer.add_char b (Char.chr byte)
  | `Fun f -> f
	
type output = { mutable encode : uchar -> unit; mutable encoding : encoding;}
           
let make_output encoding dest =
  let w = byte_writer_of_dest dest in
  let encode = match encoding with 
  | `UTF_8 -> utf8_encode w
  | `UTF_16 | `UTF_16BE -> utf16be_encode w
  | `UTF_16LE -> utf16le_encode w
  in
  { encode = encode; encoding = encoding; }
  
let output_encoding o = o.encoding
let output o u = o.encode u

(* Strings *)

let byte_reader_of_string s = 
  let len = String.length s in 
  let pos = ref 0 in 
  fun () -> 
    if !pos >= len then raise End_of_file else
    let u = Char.code (String.unsafe_get s !pos) in (incr pos; u)

let rec fold decoder r f acc = match decoder r with 
| `Eoi -> acc
| (`Malformed | `Uchar _ as c) -> fold decoder r f (f acc c)

let fold_utf8 f acc s = fold utf8_decode (byte_reader_of_string s) f acc 
let fold_utf16be f acc s = fold utf16be_decode (byte_reader_of_string s) f acc 
let fold_utf16le f acc s = fold utf16le_decode (byte_reader_of_string s) f acc 

(* Buffers *)

let byte_writer_of_buffer b byte = Buffer.add_char b (Char.chr byte)
let add_utf8 b u = utf8_encode (byte_writer_of_buffer b) u
let add_utf16be b u = utf16be_encode (byte_writer_of_buffer b) u
let add_utf16le b u = utf16le_encode (byte_writer_of_buffer b) u
  
(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
