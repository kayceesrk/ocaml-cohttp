(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

open Cohttp
open Sexplib.Std
open Sexplib.Conv

type t = [
  | Body.t
  | `Stream of string Cohttp_effect_stream.t
] with sexp

let empty = (Body.empty :> t)

let create_stream fn arg =
  let fin = ref false in
  let stream = fun () ->
    match !fin with
    | true -> None
    | false -> begin 
      match fn arg with
      | Transfer.Done -> fin := true; None
      | Transfer.Final_chunk c -> fin := true; Some c
      | Transfer.Chunk c -> Some c
      end 
  in
  Cohttp_effect_stream.mk_stream fin stream

let is_empty (body:t) =
  match body with
  | #Body.t as body -> (Body.is_empty body)
  | `Stream s -> Cohttp_effect_stream.is_empty s

let to_string (body:t) =
  match body with
  | #Body.t as body -> Body.to_string body
  |`Stream s ->
    let b = Buffer.create 1024 in
    Cohttp_effect_stream.iter (Buffer.add_string b) s;
    Buffer.contents b

let to_string_list (body:t) =
  match body with
  | #Body.t as body -> Body.to_string_list body
  |`Stream s -> Cohttp_effect_stream.to_list s

let of_string s = ((Body.of_string s) :> t)

let to_stream (body:t) =
  match body with
  |`Empty -> Cohttp_effect_stream.of_list []
  |`Stream s -> s
  |`String s -> Cohttp_effect_stream.of_list [s]
  |`Strings sl -> Cohttp_effect_stream.of_list sl

let drain_body (body:t) =
  match body with
  |`Empty
  |`String _
  |`Strings _ -> ()
  |`Stream s -> Cohttp_effect_stream.junk_while (fun _ -> true) s

let of_string_list l = `Strings l

let of_stream s = `Stream s

let transfer_encoding = function
  |#Body.t as t -> Body.transfer_encoding t
  |`Stream _ -> Transfer.Chunked

(* This will consume the body and return a length, and a
 * new body that should be used instead of the input *)
let length (body:t) : (int64 * t) =
  match body with
  |#Body.t as body -> Body.length body, body
  |`Stream s ->
    let buf = to_string body in
    let len = Int64.of_int (String.length buf) in
    len, `String buf

let write_body fn = function
  |`Empty -> ()
  |`Stream st -> Cohttp_effect_stream.iter fn st
  |`String s -> fn s
  |`Strings sl -> List.iter fn sl

let map f t =
  match t with
  | #Body.t as t -> (Body.map f t :> t)
  | `Stream s -> `Stream (Cohttp_effect_stream.map f s)
