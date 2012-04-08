(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Unicode input/output. 

  [Ucutf] defines {{:#input}input} and {{:#output}output} abstractions
  to decode and encode the {{:http://www.ietf.org/rfc/rfc3629.txt}
  UTF-8}, {{:http://www.ietf.org/rfc/rfc2781.txt} UTF-16}, UTF-16LE
  and UTF-16BE
  {{:http://unicode.org/glossary/#unicode_encoding_scheme}encoding
  schemes}. Input abstractions can track the current character line and 
  column position in the input stream and perform new line normalization.

  Functions are also provided to {{:#stringfold} fold over} the
  characters of UTF encoded OCaml string values and to
  {{:#bufferencode}directly encode} characters in OCaml buffer values.  

  See {{:#examples}examples} of use.

  {e Release %%VERSION%% - %%AUTHORS%% }

  {3 References}
    {ul 
    {- The Unicode Consortium. 
    {e {{:http://www.unicode.org/versions/latest}The Unicode Standard}}. 
    (latest version)}}
*)

  (** {1:basic Unicode characters and encoding schemes} *)

type encoding = [ `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]
(** The type for 
    {{:http://unicode.org/glossary/#character_encoding_scheme}encoding
    schemes}. *)

type uchar = int
(** The type for unicode characters.  

    More precisely a value of this type {b returned by [Ucutf]} is an
    {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
    scalar value} i.e. an integer value in the ranges [0x0000]
    ... [0xD7FF] or [0xE000] ... [0x10FFFF]. *)

val u_bom : uchar 
(** [u_bom] is the {{:http://unicode.org/glossary/#byte_order_mark}byte
    order mark} (BOM) character [U+FEFF]. *)

val u_rep : uchar 
(** [u_rep] is the
    {{:http://unicode.org/glossary/#replacement_character}replacement} 
    character [U+FFFD]. *)

val u_lf : uchar 
(** [u_lf] is the line feed character [U+000A] (['\n']). *)

val is_uchar : uchar -> bool
(** [is_uchar c] is [true] iff [c] is an 
    {{:http://unicode.org/glossary/#unicode_scalar_value}
    unicode scalar value}. *)

val cp_to_string : uchar -> string
(** [cp_to_string cp] represents the
    {{:http://unicode.org/glossary/#code_point}code point} [cp] in
    ASCII by the string ["U+n"] where n is four to six capital
    hexadecimal digits with leading zeros omitted unless the code
    point has fewer than four digits. If [cp] is not a valid code point
    ["U+Invalid(X)"] is returned where [X] is the hexadecimal integer value. 

    {b Warning.} Not thread safe. Use {!print_cp} for thread safety. *)

val print_cp : Format.formatter -> uchar -> unit
(** [print_cp ppf cp] prints the 
    {{:http://unicode.org/glossary/#code_point}code point} [cp] 
    on [ppf] like {!cp_to_string} does. *)

(** {1:input Input} *)

type pos = int * int
(** The type for input positions, line and column number. *)

type source = [ 
  | `Channel of Pervasives.in_channel 
  | `Fun of unit -> int
  | `String of int * string ]
(** The type for input sources. For [`String] starts reading at the
    given integer position. For [`Fun] the function must return the
    next {e byte} as an [int] and raise [End_of_file] if there is no
    such byte. *)
 
type input
(** The type for input abstractions. *)

val make_input : ?pos:bool -> ?nl:uchar -> ?encoding:encoding -> source -> input
(** [make_input pos nl encoding src] is an input abstraction reading from
    [src]. 

    {b Encoding and BOM handling.}
    [encoding] specifies the input encoding scheme. If
    unspecified it is guessed by trying to parse a
    {{:http://unicode.org/glossary/#byte_order_mark}byte order mark}
    (see the {{:http://www.unicode.org/faq/utf_bom.html#BOM}FAQ}). If
    that fails UTF-8 is assumed unless a "half" UTF-16 BOM was parsed
    in which case UTF16-BE seems a better guess. 

    If [encoding] is unspecified or equal to [`UTF_16] any initial
    {{:http://www.unicode.org/faq/utf_bom.html#BOM}BOM} is removed
    from the stream and not counted (in that case {!removed_bom}
    returns [true]). In all other cases it is returned as the first
    character of the stream since it should be interpreted as an
    initial zero width no-break space character.

    {b New line normalization.} If [nl] is specified then carriage
    return (CR, [U+000D]), line feed (LF, [U+000A]), next line (NEL,
    [U+0085]), line separator (LS, [U+0x2028]) and the sequence
    carriage return, line feed (CRLF, <[U+000D] [U+000A]>) are all
    normalized to the character [nl].

    {b Position tracking.} If [pos] is [true] (defaults to [false])
    the position and count of the last input character is returned by
    the functions {!pos} and {!count}. *)

val input_encoding : input -> encoding
(** [input_encoding i] is the encoding scheme of [i]. *)

val pos : input -> pos
(** [pos i] is the line, column position of the {e last} input (or
    malformed) character. If position tracking is disabled [(0,0)]
    is returned. Otherwise before any input [(1,0)] is returned.
    Each {!input} only increments the column until a new line. 
    On a new line, the line number is incremented and the column 
    set to zero (i.e. [(2,0)] after the first new line). A new line
    is any sequence of characters that would be normalized as a new line
    (see {!make_input}). 

    This behaviour can be understood as if {!input} was moving an
    insertion {e point} on the right in the data. *)

val count : input -> int
(** [count i] is the number of characters already input on [i]
    (including malformed ones). If no characters were input or
    position tracking is disabled, [0] is returned. *)

val removed_bom : input -> bool
(** [removed_bom i] is [true] iff an initial
    {{:http://unicode.org/glossary/#byte_order_mark}byte order mark}
    was parsed and removed from the input stream. See BOM handling
    documentation in {!make_input}. *)

val input : input -> [ `Uchar of uchar | `Eoi | `Malformed ]
(** [input i] returns the next unicode scalar value in [i] or [`Eoi] if
    the end of input is reached or [`Malformed] if the byte stream
    is malformed according to the input encoding scheme. In the latter
    case it is possible to continue to input characters until the stream
    synchronizes again on valid bytes (but it may be a good idea
    to signal the malformed characters by adding an {!u_rep} character
    to the parsed data, see the {{:#examples}examples}). *)

val peek : input -> [ `Uchar of uchar | `Eoi | `Malformed ]
(** [peek i] is like {!input} but it doesn't remove the character from
    the stream (and the position is not updated). *)

(** {1:output Output} *)

type dest = [ `Channel of out_channel | `Buffer of Buffer.t | 
              `Fun of (int -> unit) ]
(** The type for output destinations. For [`Buffer], the buffer won't
    be cleared. For [`Fun] the function is called with the output {e
    bytes} as [int]s. *)

type output 
(** The type for output abstractions. *)

val make_output : encoding -> dest -> output
(** [make_output encoding dest] is an output abstraction writing to
    [dest].  

    {b Note.} No initial
    {{:http://unicode.org/glossary/#byte_order_mark}byte order mark}
    is output. This duty is left to the client. *)

val output_encoding : output -> encoding 
(** [output_encoding o] is the encoding of [o]. *)

val output : output -> uchar -> unit
(** [output o u] outputs the unicode scalar value [u] on [o]. 

    {b Warning.} The function assumes that [u] is an
    {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
    scalar value}. If you are handling foreign data you
    can use {!is_uchar} to assert that. *)

(** {1:stringfold String folders} *)

val fold_utf8 : 
    ('a -> [ `Uchar of uchar | `Malformed ] -> 'a) -> 'a -> string -> 'a 
(** [fold_utf8 f a s] is 
    [f (] ... [(f (f a u]{_0}[) u]{_1}[)] ... [)] ... [) u]{_n} 
    where [u]{_i} are the
    {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
    scalar value} of the UTF-8 encoded string [s]. *)

val fold_utf16be : 
    ('a -> [ `Uchar of uchar | `Malformed ] -> 'a) -> 'a -> string -> 'a
(** [fold_utf16be f a s] is 
    [f (] ... [(f (f a u]{_0}[) u]{_1}[)] ... [)] ... [) u]{_n} 
    where [u]{_i} are the
    {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
    scalar value} of the UTF-16BE encoded string [s]. *)

val fold_utf16le : 
    ('a -> [ `Uchar of uchar | `Malformed ] -> 'a) -> 'a -> string -> 'a
(** [fold_utf16le f a s] is 
    [f (] ... [(f (f a u]{_0}[) u]{_1}[)] ... [)] ... [) u]{_n} 
    where [u]{_i} are the
    {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
    scalar value} of the UTF-16LE encoded string [s]. *)


(** {1:bufferencode Buffer encoders} 

    {b Warning.} All the functions below assumes that [u] is an
    {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
    scalar value}. If you are handling foreign data you
    can use {!is_uchar} to assert that. 
*)

val add_utf8 : Buffer.t -> uchar -> unit
(** [add_utf8 b u] adds the UTF-8 encoding of the
    {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
    scalar value} [u] to [b]. *)

val add_utf16be : Buffer.t -> uchar -> unit
(** [add_utf16be b u] adds the UTF-16BE encoding of the
    {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
    scalar value} [u] to [b]. *)

val add_utf16le : Buffer.t -> uchar -> unit
(** [add_utf16le b u] adds the UTF-16LE encoding of the
    {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
    scalar value} [u] to [b]. *)

(** {1:examples Examples} 

    The [recode src out_encoding dst] function call inputs characters from 
    [src] and outputs the result on [dst] with the
    encoding [out_encoding]. If a decoding error occurs we output the
    replacement character {!u_rep} and continue.
{[let recode ?nl ?encoding src out_encoding dst = 
  let i = Ucutf.make_input ?nl ?encoding src in 
  let o = Ucutf.make_output out_encoding dst in
  let rec copy i o = match Ucutf.input i with 
    | `Uchar u -> Ucutf.output o u; copy i o 
    | `Malformed -> Ucutf.output o Ucutf.u_rep; copy i o 
    | `Eoi -> ()
  in
  if out_encoding = `UTF_16 then Ucutf.output o Ucutf.u_bom;
  copy i o]}
    The [input_lines src] function call inputs unicode data from [src]
    and parses its lines into a list of UTF-8 encoded
    strings. Line breaks are determined according to the 
    {{:http://www.unicode.org/versions/Unicode5.2.0/}Unicode 5.2}
    recommendation R4 of section 5.8 for a [readline] function. If
    a decoding error occurs we add the replacement character {!u_rep} in 
    the current line and continue.
{[let input_lines ?encoding src =
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
  aux i b []]}
*)
    
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
