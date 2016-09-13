(*
 * Copyright (c) 2012-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazazagnaire.org>
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
 * %%NAME%% %%VERSION%%
 *)

module type CHANNEL = V1.CHANNEL
  with type 'a io = 'a 
   and type 'a io_stream = 'a Aeio.Stream.t
   and type buffer = Cstruct.t

module Make(Channel:CHANNEL) = struct

  type 'a t = 'a 
  type ic = Channel.t
  type oc = Channel.t
  type conn = Channel.flow

  let read_line ic =
    match Channel.read_line ic with
    | []   -> None
    | bufs -> Some (Cstruct.copyv bufs)

  let read ic len =
    try
      let iop = Channel.read_some ~len ic in
      Cstruct.to_string iop
    with
    | End_of_file -> "" 

  let write oc buf =
    Channel.write_string oc buf 0 (String.length buf);
    Channel.flush oc

  let flush _ =
    (* NOOP since we flush in the normal writer functions above *)
    ()

  let (>>=) = (|>)
  let return a = a

end
