(*
 * Copyright (c) 2011-2014 Anil Madhavapeddy <anil@recoil.org>
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
 *)

module type FLOW = V1.FLOW
  with type 'a io = 'a
   and type buffer = Cstruct.t

module type CHANNEL = V1.CHANNEL
  with type 'a io = 'a 
   and type 'a io_stream = 'a Aeio.Stream.t
   and type buffer = Cstruct.t

module Make(F:FLOW) : sig
  include CHANNEL with type flow = F.flow
  exception Read_error of F.error
  exception Write_error of F.error

  val read_exactly: len:int -> t -> Cstruct.t list io
  (** [read_exactly len t] reads [len] bytes from the channel [t] or fails
      with [Read_error] or [End_of_file]. *)
end