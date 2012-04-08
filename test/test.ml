(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)
let str = Printf.sprintf
let pr = Printf.printf 

let codec_test () = 
  pr "Codec every unicode scalar value in UTF-8, UTF16-BE and UTF16LE.\n%!";
  let b = Buffer.create 4 in 
  let codec scheme enc dec b = 
    let codec_u scheme enc dec b u = 
      let s = Buffer.clear b; enc b u; Buffer.contents b in 
      match dec (fun acc c -> c :: acc) [] s with
      | [ `Uchar u' ] when u = u' -> ()
      | _ -> failwith  
	    (str "%s codec failed for %s" scheme (UCutf.cp_to_string u))
    in
    for u = 0x0000 to 0xD7FF do codec_u scheme enc dec b u done;
    for u = 0xE000 to 0x10FFFF do codec_u scheme enc dec b u  done
  in
  codec "UTF-8" UCutf.add_utf8 UCutf.fold_utf8 b;
  codec "UTF-16BE" UCutf.add_utf16be UCutf.fold_utf16be b;
  codec "UTF-16LE" UCutf.add_utf16le UCutf.fold_utf16le b

let pos_test () = 
  pr "Test position tracking.\n%!";
  let encoding = `UTF_8 in 
  let pos = true in 
  let s = "LL\nL\r\nLL\r\n\n" in
  let i = UCutf.make_input ~pos ~encoding (`String (0, s)) in
  assert (UCutf.pos i = (1,0) && UCutf.count i = 0); ignore (UCutf.input i); 
  assert (UCutf.pos i = (1,1) && UCutf.count i = 1); ignore (UCutf.input i);
  assert (UCutf.pos i = (1,2) && UCutf.count i = 2); ignore (UCutf.input i);
  assert (UCutf.pos i = (2,0) && UCutf.count i = 3); ignore (UCutf.input i);
  assert (UCutf.pos i = (2,1) && UCutf.count i = 4); ignore (UCutf.input i);
  assert (UCutf.pos i = (3,0) && UCutf.count i = 5); ignore (UCutf.input i);
  assert (UCutf.pos i = (3,0) && UCutf.count i = 6); ignore (UCutf.input i);
  assert (UCutf.pos i = (3,1) && UCutf.count i = 7); ignore (UCutf.input i);
  assert (UCutf.pos i = (3,2) && UCutf.count i = 8); ignore (UCutf.input i);
  assert (UCutf.pos i = (4,0) && UCutf.count i = 9); ignore (UCutf.input i);
  assert (UCutf.pos i = (4,0) && UCutf.count i = 10); ignore (UCutf.input i);
  assert (UCutf.pos i = (5,0) && UCutf.count i = 11); ignore (UCutf.input i);
  assert (UCutf.pos i = (5,0) && UCutf.count i = 11); ignore (UCutf.input i);
  assert (UCutf.pos i = (5,0) && UCutf.count i = 11);
  let i = UCutf.make_input ~nl:UCutf.u_lf ~pos ~encoding (`String (0, s)) in
  assert (UCutf.pos i = (1,0) && UCutf.count i = 0); ignore (UCutf.input i); 
  assert (UCutf.pos i = (1,1) && UCutf.count i = 1); ignore (UCutf.input i);
  assert (UCutf.pos i = (1,2) && UCutf.count i = 2); ignore (UCutf.input i);
  assert (UCutf.pos i = (2,0) && UCutf.count i = 3); ignore (UCutf.input i);
  assert (UCutf.pos i = (2,1) && UCutf.count i = 4); ignore (UCutf.input i);
  assert (UCutf.pos i = (3,0) && UCutf.count i = 5); ignore (UCutf.input i);
  assert (UCutf.pos i = (3,1) && UCutf.count i = 6); ignore (UCutf.input i);
  assert (UCutf.pos i = (3,2) && UCutf.count i = 7); ignore (UCutf.input i);
  assert (UCutf.pos i = (4,0) && UCutf.count i = 8); ignore (UCutf.input i);
  assert (UCutf.pos i = (5,0) && UCutf.count i = 9); ignore (UCutf.input i);
  assert (UCutf.pos i = (5,0) && UCutf.count i = 9); ignore (UCutf.input i);
  assert (UCutf.pos i = (5,0) && UCutf.count i = 9);
  ()

let utf16be_test () =
  let malformed () = function `Malformed -> raise Exit | _ -> () in
  try UCutf.fold_utf16be malformed () "\xFE\xFF\xFF"; assert false
  with Exit -> ()

let utf16le_test () =
  let malformed () = function `Malformed -> raise Exit | _ -> () in
  try UCutf.fold_utf16le malformed () "\xFF\xFE\xFD"; assert false
  with Exit -> ()

(* Well-formed UTF-8 byte sequences (cf. table 3.7 p. 104 Unicode 5.0) *)
let utf8_wf_spec = [ 
  (0x0000,0x007F),     [(0x00,0x7F)];
  (0x0080,0x07FF),     [(0xC2,0xDF); (0x80,0xBF)];
  (0x0800,0x0FFF),     [(0xE0,0xE0); (0xA0,0xBF); (0x80,0xBF)];
  (0x1000,0xCFFF),     [(0xE1,0xEC); (0x80,0xBF); (0x80,0xBF)];
  (0xD000,0xD7FF),     [(0xED,0xED); (0x80,0x9F); (0x80,0xBF)];
  (0xE000,0xFFFF),     [(0xEE,0xEF); (0x80,0xBF); (0x80,0xBF)];
  (0x10000,0x3FFFF),   [(0xF0,0xF0); (0x90,0xBF); (0x80,0xBF); (0x80,0xBF)];
  (0x40000,0xFFFFF),   [(0xF1,0xF3); (0x80,0xBF); (0x80,0xBF); (0x80,0xBF)];
  (0x100000,0x10FFFF), [(0xF4,0xF4); (0x80,0x8F); (0x80,0xBF); (0x80,0xBF)]]

let utf8_wf_assoc utf8_wf = (* (uchar * byte sequence) list from spec *)
  let range (a,b) = 
    let acc = ref [] in for i = b downto a do acc := i :: !acc done; !acc
  in
  let rec bytes_of_spec = function
    | [] -> [""]
    | (min, max) :: rest ->
	let acc = ref [] in
	let push i s = (String.make 1 (Char.chr i)) ^ s in
	let seqs = bytes_of_spec rest in
	for i = min to max do
	  List.iter (fun s -> acc := (push i s) :: !acc) seqs
	done;
	List.rev !acc
  in
  let u_range_spec (urange, seq_spec) = 
    let bseqs = bytes_of_spec seq_spec in
    let useq = range urange in 
    List.rev (List.rev_map2 (fun u seq -> (u, seq)) useq bseqs)
  in
  let flatten ll = (* tail rec flatten *)
    List.rev (List.fold_left (fun acc l -> List.rev_append l acc) [] ll)
  in
  flatten (List.map u_range_spec utf8_wf)
  
module Int = struct type t = int let compare : int -> int -> int = compare end
module Imap = Map.Make (Int)
let utf8_encode_test assoc =      (* test the encoding of all scalar values. *)
  let buf = Buffer.create 4 in
  let umap = 
    List.fold_left (fun acc (k, v) -> Imap.add k v acc) Imap.empty assoc 
  in
  let test u = 
    let seq = try Imap.find u umap with Not_found -> assert false in
    Buffer.clear buf; UCutf.add_utf8 buf u; 
    if seq <> Buffer.contents buf then 
      failwith (str "encoding error %s" (UCutf.cp_to_string u))
  in
  for i = 0x0000 to 0xD7FF do test i done;
  for i = 0xE000 to 0x10FFFF do test i done

module Smap = Map.Make (String)
let utf8_decode_test assoc =
  let smap = 
    List.fold_left (fun acc (v, k) -> Smap.add k v acc) Smap.empty assoc 
  in
  let test seq = 
    match 
      List.rev (UCutf.fold_utf8 (fun acc c -> c :: acc) [] seq)
    with 
    | [ `Uchar u ] -> 
	begin try 
	  let u' = Smap.find seq smap in
	  if u = u' then `Decoded else
	  failwith (str "decoded %S as %s but should be %s" 
		      seq (UCutf.cp_to_string u) (UCutf.cp_to_string u'))
      with
	| Not_found -> failwith (str "decoded invalid bytes %S" seq)
	end
    | `Malformed :: _ -> 
	begin try 
	ignore (Smap.find seq smap); 
	  failwith (str "decoded %S as malformed" seq) 
	with
	| Not_found -> `Malformed
	end
    | _ -> failwith (str "this should not have happened on %S." seq)
  in
  let s1 = "X" in (* avoid too much allocation *)
  let s2 = "XX" in
  let s3 = "XXX" in
  let s4 = "XXXX" in
  for b0 = 0x00 to 0xFF do 
    s1.[0] <- Char.chr b0;
    if test s1 <> `Decoded then
      begin 
	s2.[0] <- s1.[0];
	for b1 = 0x00 to 0xFF do
	  s2.[1] <- Char.chr b1; 
	  if test s2 <> `Decoded then
	    begin 
	      s3.[0] <- s2.[0]; s3.[1] <- s2.[1];
	      for b2 = 0x00 to 0xFF do
		s3.[2] <- Char.chr b2; 
		if test s3 <> `Decoded then
		  begin 
		    s4.[0] <- s3.[0]; s4.[1] <- s3.[1]; s4.[2] <- s3.[2];
		    for b3 = 0x00 to 0xFF do
		      s4.[3] <- Char.chr b3;
		      ignore (test s4)
		    done;
		  end
	      done;
	    end
	done;
      end
  done

let utf8_test decode = 
  pr "Building uchar * UTF-8 sequence assoc list from spec.\n%!";
  let assoc = utf8_wf_assoc utf8_wf_spec in
  pr "Testing the UTF-8 encoding of every unicode scalar value.\n%!";
  utf8_encode_test assoc;
  if (decode) then begin 
    pr "Testing the UTF-8 decoding of every 4 bytes sequences \
        (very long, last test).\n%!";
      utf8_decode_test assoc;
  end;
  ()
  
let test () = 
  Printexc.record_backtrace true;
  codec_test ();
  pos_test ();
  utf16be_test ();
  utf16le_test ();
  utf8_test (Array.length Sys.argv > 1);
  print_endline "All tests succeeded.";
;;

if not (!Sys.interactive) then test ()

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
