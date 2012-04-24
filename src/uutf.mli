(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Non-blocking streaming Unicode codec.

  [Uutf] is a non-blocking streaming codec to {{:#decode}decode} and
  {{:#encode}encode} the {{:http://www.ietf.org/rfc/rfc3629.txt}
  UTF-8}, {{:http://www.ietf.org/rfc/rfc2781.txt} UTF-16}, UTF-16LE
  and UTF-16BE encoding schemes. It can efficiently work character by
  character without blocking on IO. Decoders perform 
  character position tracking and support {{!nln}newline normalization}.

  Functions are also provided to {{!String} fold over} the
  characters of UTF encoded OCaml string values and to
  {{!Buffer}directly encode} characters in OCaml {!Buffer.t}
  values.

  See {{:#examples}examples} of use.

  {e Release %%VERSION%% - %%AUTHORS%% }

  {3 References}
    {ul 
    {- The Unicode Consortium. 
    {e {{:http://www.unicode.org/versions/latest}The Unicode Standard}}. 
    (latest version)}}
*)

  (** {1:basic Unicode characters} 

      [Uutf] uses the term character for a Unicode
      {{:http://unicode.org/glossary/#unicode_scalar_value} scalar
      value} which is an integer value in the ranges [0x0000]
      ... [0xD7FF] and [0xE000] ... [0x10FFFF]. This should not be
      confused with a Unicode
      {{:http://unicode.org/glossary/#code_point}code point}, which is
      a scalar value or a (textually meaningless)
      {{:http://unicode.org/glossary/#surrogate_code_point}surrogate
      code point}. *)

type uchar = int
(** The type for Unicode characters. Any value of this type returned
    by [Uutf] is a Unicode 
    {{:http://unicode.org/glossary/#unicode_scalar_value}
    scalar value}. *)

val u_bom : uchar 
(** [u_bom] is the {{:http://unicode.org/glossary/#byte_order_mark}byte
    order mark} (BOM) character ([U+FEFF]). *)

val u_rep : uchar 
(** [u_rep] is the
    {{:http://unicode.org/glossary/#replacement_character}replacement} 
    character ([U+FFFD]). *)

val is_uchar : int -> bool
(** [is_uchar cp] is [true] iff [cp] is a Unicode
    {{:http://unicode.org/glossary/#unicode_scalar_value}
    scalar value}. *)

val cp_to_string : int -> string
(** [cp_to_string cp] represents the
    {{:http://unicode.org/glossary/#code_point}code point} [cp] in
    ASCII according to the Unicode notational convention
    (see Appendix A in Unicode 6.0.0).
    If [cp] is not a valid code point ["U+Invalid(X)"] is
    returned where [X] is the hexadecimal integer value.

    {b Warning.} Not thread safe. Use {!print_cp} for thread safety. *)

val print_cp : Format.formatter -> int -> unit
(** [print_cp ppf cp] prints [cp] on [ppf]. See {!cp_to_string}. *)

(** {1:schemes Unicode encoding schemes} *)

type encoding = [ `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]
(** The type for Unicode
    {{:http://unicode.org/glossary/#character_encoding_scheme}encoding
    schemes}. *)

type decoder_encoding = [ encoding | `US_ASCII | `ISO_8859_1 ]
(** The type for encoding schemes {e decoded} by [Uutf]. Unicode encoding 
    schemes plus {{:http://tools.ietf.org/html/rfc20}US-ASCII} and 
    {{:http://www.ecma-international.org/publications/standards/Ecma-094.htm}
    ISO/IEC 8859-1} (latin-1). *)

val encoding_of_string : string -> decoder_encoding option
(** [encoding_of_string s] converts a (case insensitive)
    {{:http://www.iana.org/assignments/character-sets}IANA character set name}
    to an encoding. *)

val encoding_to_string : [< decoder_encoding] -> string
(** [encoding_to_string e] is a
    {{:http://www.iana.org/assignments/character-sets}IANA character set name}
    for [e]. *)

(** {1:decode Decode} *)

type src = [ `Channel of in_channel | `String of string | `Manual ]
(** The type for input sources. With a [`Manual] source the client 
    must provide input with {!Manual.src}. *)

type nln = [ `ASCII of uchar | `NLF of uchar | `Readline of uchar ]
(** The type for newline normalizations. The variant argument is the 
    normalization character.
    {ul
    {- [`ASCII], normalizes CR ([U+000D]), LF ([U+000A]) and CRLF
       (<[U+000D] [U+000A]>).}
    {- [`NLF], normalizes the Unicode newline function (NLF). This is
       NEL ([U+0085]) and the normalizations of [`ASCII].}
    {- [`Readline], normalizes for a Unicode readline function. This is FF
       ([U+000C]), LS ([U+2028]), PS ([U+2029]), and the normalizations 
       of [`NLF].}}
    Used with an appropriate normalization character the [`NLF] and 
    [`Readline] normalizations allow to implement all the different 
    recommendations of Unicode's newline guidelines (section 5.8 in 
    Unicode 6.0.0). *)

type decoder
(** The type for decoders. *)

val decoder : ?nln:nln -> ?encoding:[< decoder_encoding] -> [< src] -> decoder
(** [decoder nln encoding src] is a decoder that inputs from [src].

    {b Byte order mark.}
    {{:http://unicode.org/glossary/#byte_order_mark}Byte order mark}
    (BOM) constraints are application dependent and prone to
    misunderstandings (see the
    {{:http://www.unicode.org/faq/utf_bom.html#BOM}FAQ}). Hence,
    [Uutf] decoders have a simple rule: an {e initial BOM is always
    removed from the input and not counted in character position
    tracking}. The function {!decoder_removed_bom} does however return
    [true] if a BOM was removed so that all the information can be
    recovered if needed.

    For UTF-16BE and UTF-16LE the above rule is a violation of
    conformance D96 and D97 of the standard. [Uutf] favors the idea
    that if there's a BOM, decoding with [`UTF_16] or the [`UTF_16XX]
    corresponding to the BOM should decode the same character sequence
    (this is not the case if you stick to the standard). The client
    can however regain conformance by consulting the result of
    {!decoder_removed_bom} and take appropriate action.

    {b Encoding.} [encoding] specifies the decoded encoding
    scheme. If [`UTF_16] is used the endianness is determined
    according to the standard: from a
    {{:http://unicode.org/glossary/#byte_order_mark}BOM}
    if there is one, [`UTF_16BE] otherwise.

    If [encoding] is unspecified it is guessed. The result of a guess
    can only be [`UTF_8], [`UTF_16BE] or [`UTF_16LE]. The heuristic
    looks at the first three bytes of input (or less if impossible)
    and takes the {e first} matching byte pattern in the table below.
{[
xx = any byte
.. = any byte or no byte (input too small)
pp = positive byte 
uu = valid UTF-8 first byte

Bytes    | Guess     | Rationale
---------+-----------+-----------------------------------------------
EF BB BF | `UTF_8    | UTF-8 BOM
FE FF .. | `UTF_16BE | UTF-16BE BOM
FF FE .. | `UTF_16LE | UTF-16LE BOM
00 pp .. | `UTF_16BE | ASCII UTF-16BE and U+0000 is often forbidden
pp 00 .. | `UTF_16LE | ASCII UTF-16LE and U+0000 is often forbidden
uu .. .. | `UTF_8    | ASCII UTF-8 or valid UTF-8 first byte.
xx xx .. | `UTF_16BE | Not UTF-8 => UTF-16, no BOM => UTF-16BE
.. .. .. | `UTF_8    | Single malformed UTF-8 byte or no input. 
]}
    This heuristic is compatible both with BOM based
    recognitition and
    {{:http://tools.ietf.org/html/rfc4627#section-3}JSON-like encoding
    recognition} that relies on ASCII being present at the beginning
    of the stream. Also, {!decoder_removed_bom} will tell the client
    if the guess was BOM based.
    
    {b Newline normalization.} If [nln] is specified, the given
    newline normalization is performed, see {!nln}. Otherwise
    all newlines are returned as found in the input.

    {b Character position.} The line number, column number and
    character count of the last decoded character (including
    [`Malformed] ones) are respectively returned by {!decoder_line},
    {!decoder_col} and {!decoder_count}. Before the first call to
    {!decode} the line number is [1] and the column is [0].  Each
    {!decode} returning [`Uchar] or [`Malformed] increments the column
    until a newline.  On a newline, the line number is incremented and
    the column set to zero. For example the line is [2] and column [0]
    after the first newline was decoded. This can be understood as if {!decode}
    was moving an insertion point to the right in the data.  A {e
    newline} is anything normalized by [`Readline], see {!nln}. *)

val decode : decoder -> 
  [ `Await | `Uchar of uchar | `End | `Malformed of string]
(** [decode d] is:
    {ul
    {- [`Await] if [d] has a [`Manual] input source and awaits 
       for more input. The client must use {!Manual.src} to provide it.}
    {- [`Uchar u] if a Unicode scalar value [u] was decoded.}
    {- [`End] if the end of input was reached.}
    {- [`Malformed bytes] if the [bytes] sequence is malformed according to 
       the decoded encoding scheme. If you are interested in a best-effort
       decoding you can still continue to decode after an error until the
       decoder synchronizes again on valid bytes. It may however be a good
       idea to signal the malformed characters by adding an {!u_rep}
       character to the parsed data, see the {{:#examples}examples}.}} 

    {b Note.} Repeated invocation always eventually returns [`End], even
    in case of errors.  *)

val decoder_encoding : decoder -> decoder_encoding
(** [decoder_encoding d] is [d]'s the decoded encoding scheme of [d]. 

    {b Warning.} If the decoder guesses the encoding or uses [`UTF_16], 
    rely on this value only after the first [`Uchar] was decoded. *)

(**/**)

(* This function is dangerous, it may destroy the current continuation. 
   But it's needed for things like XML parsers. *)

val set_decoder_encoding : decoder -> [< decoder_encoding] -> unit
(** [set_decoder_encoding d enc] changes the decoded encoding 
    to [enc] after decoding started.
    
    {b Warning.} Call only after {!decode} was called on [d] and that the 
    last call to it returned something different from [`Await] or data may
    be lost. After encoding guess wait for at least three [`Uchar]s.
*)

(**/**)

val decoder_line : decoder -> int
(** [decoder_line d] is the line number of the last
    decoded (or malformed) character. See {!decoder} for details. *)

val decoder_col : decoder -> int
(** [decoder_col d] is the column number of the last decoded 
    (or malformed) character. See {!decoder} for details. *)
    
val decoder_count : decoder -> int
(** [decoder_count d] is the number of characters already decoded on [d]
    (including malformed ones). See {!decoder} for details. *)

val decoder_removed_bom : decoder -> bool
(** [decoder_removed_bom d] is [true] iff an {e initial}
    {{:http://unicode.org/glossary/#byte_order_mark}BOM} was
    removed from the input stream. See {!decoder} for details. *)

val decoder_src : decoder -> src
(** [decoder_src d] is [d]'s input source. *)

val decoder_nln : decoder -> nln option
(** [decoder_nln d] returns [d]'s newline normalization (if any). *)

(** {1:encode Encode} *)

type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
(** The type for output destinations. With a [`Manual] destination the client
    must provide output storage with {!Manual.dst}. *)

type encoder
(** The type for Unicode encoders. *)

val encoder : [< encoding] -> [< dst] -> encoder
(** [encoder encoding dst] is an encoder for [encoding] that outputs
    to [dst]. 

    {b Note.} No initial
    {{:http://unicode.org/glossary/#byte_order_mark}BOM}
    is encoded. If needed, this duty is left to the client. *)

val encode : encoder -> [`Await | `End | `Uchar of uchar ] -> [`Ok | `Partial ]
(** [encode e v] is :
    {ul
    {- [`Partial] iff [e] has a [`Manual] destination and needs more output
       storage. The client must use {!Manual.dst} to provide a new buffer
       and then call {!encode} with [`Await] until [`Ok] is returned.}
    {- [`Ok] when the encoder is ready to encode a new [`Uchar] or [`End]}}

    For [`Manual] destination, encoding [`End] always returns
    [`Partial], the client should continue as usual with [`Await]
    until [`Ok] is returned at which point {!Manual.dst_rem} [e] is
    guaranteed to be the size of the last provided buffer (i.e. nothing
    was written).

    {b Warning.} The function assumes that [u] is a Unicode
    {{:http://unicode.org/glossary/#unicode_scalar_value}scalar value}. 
    If you are handling foreign data you can use {!is_uchar} to assert that. 

    {b Raises.} [Invalid_argument] if an [`Uchar] or [`End] is encoded
    after a [`Partial] encode. *)

val encoder_encoding : encoder -> encoding 
(** [encoder_encoding e] is [e]'s encoding. *)

val encoder_dst : encoder -> dst
(** [encoder_dst e] is [e]'s output destination. *)

(** {1:manual Manual sources and destinations.} 
    
    The functions in this module are only for decoders and encoders
    that have a [`Manual] source or destination. *)

(** Manual sources and destinations. *)
module Manual : sig
  val src : decoder -> string -> int -> int -> unit
  (** [src d s j l] provides [d] with [l] bytes to read, starting at 
      [j] in [s]. This byte range is read by calls to {!decode} with [d] 
      until [`Await] is returned. To signal the end of input call the function
      with [l = 0]. 
      
      {b Warning.} Do not use with non-[`Manual] decoder sources. *)

  val dst : encoder -> string -> int -> int -> unit
  (** [dst e s j l] provides [e] with [l] bytes to write, starting
      at [j] in [s]. This byte range is written by calls to {!encode} with [e]
      until [`Partial] is returned. Use {!dst_rem} to know the remaining
      number of non-written free bytes in [s]. 

      {b Warning.} Do not use with non-[`Manual] encoder destinations. *)

  val dst_rem : encoder -> int 
  (** [dst_rem e] is the remaining number of non-written, free bytes
      in the last buffer provided with {!Manual.dst}. *)
end

(** {1:strbuf String folders and Buffer encoders} *)

(** Fold over the characters of UTF encoded OCaml [string] values. *)
module String : sig

(** {1 Encoding guess} *)

  val encoding_guess : string -> [ `UTF_8 | `UTF_16BE | `UTF_16LE ] * bool
  (** [encoding_guess s] is the encoding guessed for [s] coupled with 
      [true] iff there's an initial 
      {{:http://unicode.org/glossary/#byte_order_mark}BOM}. *)

(** {1 String folders} 

    {b Note.} Initial {{:http://unicode.org/glossary/#byte_order_mark}BOM}s 
    are also folded over. *) 

  type 'a folder = 'a -> int -> [ `Uchar of uchar | `Malformed of string ] -> 
    'a 
  (** The type for character folders. The integer is the index in the 
      string where the [`Uchar] or [`Malformed] starts. *)

  val fold_utf_8 : 'a folder -> 'a -> string -> 'a 
  (** [fold_utf_8 f a s] is 
      [f (] ... [(f (f a 0 u]{_0}[) j]{_1}[ u]{_1}[)] ... [)] ... [) 
      j]{_i}[ u]{_n}.
      where [u]{_i}, [j]{_i} are the Unicode
      {{:http://unicode.org/glossary/#unicode_scalar_value} scalar value} 
      and the starting position of the characters in the 
      UTF-8 encoded string [s]. *)

  val fold_utf_16be : 'a folder -> 'a -> string -> 'a
  (** [fold_utf_16be f a s] is 
      [f (] ... [(f (f a 0 u][) j]{_1}[ u]{_1}[)] ... [)] ... [)
      j]{_i}[ u]{_n}.
      where [u]{_i}, [j]{_i} are the Unicode
      {{:http://unicode.org/glossary/#unicode_scalar_value}scalar value} 
      and the starting position of the characters in the 
      UTF-16BE encoded string [s]. *)

  val fold_utf_16le : 'a folder -> 'a -> string -> 'a
  (** [fold_utf_16le f a s] is 
      [f (] ... [(f (f a 0 u]{_0}[) j]{_1}[ u]{_1}[)] ... [)] ... [)
      j]{_i}[ u]{_n}.
      where [u]{_i}, [j]{_i} are the Unicode
      {{:http://unicode.org/glossary/#unicode_scalar_value}scalar value} 
      and the starting position of the characters in the 
      UTF-16LE encoded string [s]. *)
end

(**  UTF encode characters in OCaml {!Buffer.t} values. *)
module Buffer : sig 

  (** {1 Buffer encoders} 

      {b Warning.} All the functions below assumes that [u] is a
      {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
      scalar value}. If you are handling foreign data you
      can use {!is_uchar} to assert that. *)

  val add_utf_8 : Buffer.t -> uchar -> unit
  (** [add_utf_8 b u] adds the UTF-8 encoding of the
      {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
      scalar value} [u] to [b]. *)

  val add_utf_16be : Buffer.t -> uchar -> unit
  (** [add_utf_16be b u] adds the UTF-16BE encoding of the
      {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
      scalar value} [u] to [b]. *)

  val add_utf_16le : Buffer.t -> uchar -> unit
  (** [add_utf_16le b u] adds the UTF-16LE encoding of the
      {{:http://unicode.org/glossary/#unicode_scalar_value} unicode
      scalar value} [u] to [b]. *)
end

(** {1:examples Examples} 

    {2:readlines Read lines}

    The value of [lines src] is the list of lines in [src] as UTF-8
    encoded OCaml strings. Line breaks are determined according to the
    recommendation R4 for a [readline] function in section 5.8 of
    Unicode 6.0.0. If a decoding error occurs we silently replace the
    malformed sequence by the replacement character {!u_rep} and continue.
{[let lines ?encoding (src : [`Channel of in_channel | `String of string]) =
  let rec loop d buf acc = match Uutf.decode d with 
  | `Uchar 0x000A -> 
      let line = Buffer.contents buf in
      Buffer.clear buf; loop d buf (line :: acc)
  | `Uchar u -> Uutf.Buffer.add_utf_8 buf u; loop d buf acc
  | `End -> List.rev (Buffer.contents buf :: acc)  
  | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop d buf acc
  | `Await -> assert false
  in
  let nln = `Readline 0x000A in
  loop (Uutf.decoder ~nln ?encoding src) (Buffer.create 512) []]}
  
  Using the [`Manual] interface, [lines_fd] does the same but on a Unix file 
  descriptor. 
{[let lines_fd ?encoding (fd : Unix.file_descr) =
  let rec loop fd s d buf acc = match Uutf.decode d with 
  | `Uchar 0x000A -> 
      let line = Buffer.contents buf in
      Buffer.clear buf; loop fd s d buf (line :: acc)
  | `Uchar u -> Uutf.Buffer.add_utf_8 buf u; loop fd s d buf acc
  | `End -> List.rev (Buffer.contents buf :: acc)
  | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop fd s d buf acc
  | `Await ->
      let rec unix_read fd s j l = try Unix.read fd s j l with 
      | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l 
      in
      let rc = unix_read fd s 0 (String.length s) in 
      Uutf.Manual.src d s 0 rc; loop fd s d buf acc
  in
  let s = String.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let nln = `Readline 0x000A in
  loop fd s (Uutf.decoder ~nln ?encoding `Manual) (Buffer.create 512) []
]}

    {2:recode Recode}

    The result of [recode src out_encoding dst] has the characters of
    [src] written on [dst] with encoding [out_encoding].  If a
    decoding error occurs we silently replace the malformed sequence
    by the replacement character {!u_rep} and continue.  Note that we
    don't add an initial
    {{:http://unicode.org/glossary/#byte_order_mark}BOM} to [dst], 
    recoding will thus loose the initial BOM [src] may have. Whether
    this is a problem or not depends on the context.
{[let recode ?nln ?encoding out_encoding 
    (src : [`Channel of in_channel | `String of string])
    (dst : [`Channel of out_channel | `Buffer of Buffer.t])
  = 
  let rec loop d e = match Uutf.decode d with 
  | `Uchar _ as u -> ignore (Uutf.encode e u); loop d e 
  | `End -> ignore (Uutf.encode e `End)
  | `Malformed _ -> ignore (Uutf.encode e (`Uchar Uutf.u_rep)); loop d e 
  | `Await -> assert false
  in
  let d = Uutf.decoder ?nln ?encoding src in 
  let e = Uutf.encoder out_encoding dst in
  loop d e]}
  Using the [`Manual] interface, [recode_fd] does the same but between
  Unix file descriptors.
{[let recode_fd ?nln ?encoding out_encoding 
    (fdi : Unix.file_descr) 
    (fdo : Unix.file_descr) 
  = 
  let rec encode fd s e v = match Uutf.encode e v with `Ok -> () 
  | `Partial -> 
      let rec unix_write fd s j l = 
        let rec write fd s j l = try Unix.single_write fd s j l with 
        | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s j l 
        in
        let wc = write fd s j l in 
        if wc < l then unix_write fd s (j + wc) (l - wc) else ()
      in
      unix_write fd s 0 (String.length s - Uutf.Manual.dst_rem e); 
      Uutf.Manual.dst e s 0 (String.length s);
      encode fd s e `Await
  in
  let rec loop fdi fdo ds es d e = match Uutf.decode d with 
  | `Uchar _ as u -> encode fdo es e u; loop fdi fdo ds es d e 
  | `End -> encode fdo es e `End
  | `Malformed _ -> encode fdo es e (`Uchar Uutf.u_rep); loop fdi fdo ds es d e 
  | `Await ->
      let rec unix_read fd s j l = try Unix.read fd s j l with 
      | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l 
      in
      let rc = unix_read fdi ds 0 (String.length ds) in 
      Uutf.Manual.src d ds 0 rc; loop fdi fdo ds es d e 
  in
  let ds = String.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let es = String.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let d = Uutf.decoder ?nln ?encoding `Manual in 
  let e = Uutf.encoder out_encoding `Manual in
  Uutf.Manual.dst e es 0 (String.length es);
  loop fdi fdo ds es d e]}
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
