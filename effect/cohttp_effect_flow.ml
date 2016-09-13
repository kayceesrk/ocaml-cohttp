type +'a io = 'a
type buffer = Cstruct.t

type flow =  Unix.file_descr

type error = exn

let error_message = Printexc.to_string

module Cstruct = struct
  include Cstruct

  let of_bytes ?allocator buf =
    let buflen = Bytes.length buf in
    match allocator with
    |None ->
      let c = create buflen in
      blit_from_string buf 0 c 0 buflen;
      c
    |Some fn ->
      let c = fn buflen in
      blit_from_string buf 0 c 0 buflen;
      set_len c buflen
end

let read fd =
  (* Fixme: Really inefficient this *)
  let byte_buffer = Bytes.create 32 in
  try
    let recvlen = Aeio.recv fd byte_buffer 0 32 [] in
    if recvlen = 0 then `Eof
    else `Ok (Cstruct.of_bytes (Bytes.sub byte_buffer 0 recvlen))
  with
  | e -> `Error e

let send sock str =
  let len = Bytes.length str in
  let total = ref 0 in
  begin try
      while !total < len do
        let write_count = Aeio.send sock str !total (len - !total) [] in
        total := write_count + !total
      done; 
      `Ok ()
    with 
    | e -> `Error e
  end

let write fd buffer = 
  let len = Cstruct.len buffer in
  let byte_buffer = Bytes.create len in
  Cstruct.blit_to_bytes buffer 0 byte_buffer 0 len;
  send fd byte_buffer

let writev fd bl = 
  let res = List.map (fun b -> write fd b) bl in
  try
    List.iter (fun v -> match v with
    | `Ok _ -> ()
    | `Error e -> raise e
    | `Eof -> failwith "writev: unexpected") res;
    `Ok ()
  with
  | e -> `Error e

let close = Unix.close 
