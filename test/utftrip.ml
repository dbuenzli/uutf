(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let exec = Filename.basename Sys.executable_name
let pr_err s = Printf.eprintf "%s: %s\n" exec s
  
let trip nln ienc oenc = 
  let nl = if nln then Some UCutf.u_lf else None in
  let i = UCutf.make_input ?nl ?encoding:ienc (`Channel stdin) in
  let o = 
    let e = match oenc with Some e -> e | None -> UCutf.input_encoding i in
    UCutf.make_output e (`Channel stdout)
  in
  let rec aux i o = match UCutf.input i with 
  | `Uchar u -> UCutf.output o u; aux i o 
  | `Malformed -> UCutf.output o UCutf.u_rep; aux i o 
  | `Eoi -> ()
  in
  if UCutf.output_encoding o = `UTF_16 then UCutf.output o UCutf.u_bom;
  aux i o

let encoding_of_str enc = match (String.lowercase enc) with
| "" -> None
| "utf-8" | "utf8" | "utf_8" -> Some `UTF_8
| "utf-16" | "utf16" | "utf_16" -> Some `UTF_16
| "utf-16be" | "utf16-be" | "utf16be" | "utf16_be" -> Some `UTF_16BE
| "utf-16le" | "utf16-le" | "utf16le" | "utf16_le" -> Some `UTF_16LE
| e -> pr_err (str "unknown encoding '%s', trying to guess." e); None

let main () = 
  let usage = 
    str "Usage: %s <options>\n\
         \ Reads UTF data on standard input and outputs it on stdout.\n\
         \ If no input encoding is specified, it is guessed. If no output\n\
         \ encoding is specified, same as input.\n\
         Options:" exec
  in
  let ienc = ref "" in 
  let oenc = ref "" in
  let nln = ref false in
  let options = [ 
    "-ienc", Arg.Set_string ienc,
    "<enc>, input encoding, utf-8, utf-16, utf-16be, utf-16le.";
    "-oenc", Arg.Set_string oenc,
    "<enc>, output encoding, utf-8, utf-16, utf-16be, utf-16le.";
    "-nln", Arg.Set nln,
    "perform new line normalization to line feed (U+000A)."; ]
  in
  Arg.parse options (fun _ -> ()) usage;
  let ienc = encoding_of_str !ienc in 
  let oenc = encoding_of_str !oenc in
  try trip !nln ienc oenc with
  | Sys_error e -> (pr_err e; exit 1)

let () = main ()
  
(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
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

   3. Neither the name of the Daniel C. Bünzli nor the names of
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
