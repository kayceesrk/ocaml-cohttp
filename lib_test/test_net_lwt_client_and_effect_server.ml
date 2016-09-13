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

open Printf

open Cohttp

let port = 8081
let address = "127.0.0.1"
let url = Uri.of_string (sprintf "http://%s:%d/post" address port)
let url_shutdown = Uri.of_string (sprintf "http://%s:%d/shutdown" address port)

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (inet,port) ->
      (Unix.string_of_inet_addr inet) ^ ":" ^ (string_of_int port)

let make_server () =
  let module Server = Cohttp_effect.Make_server
                     (Cohttp_effect_io.Make(
                     (Cohttp_effect_channel.Make(
                     (Cohttp_effect_flow)))))
  in
  let callback conn_id req body =
    match Uri.path (Request.uri req) with
    |""|"/" ->
        Server.respond_string ~status:`OK ~body:"helloworld" ()
    |"/post" -> begin
      let body = Cohttp_effect_body.to_string body in
        Server.respond_string ~status:`OK ~body ()
    end
    |"/postnodrain" ->
        Server.respond_string ~status:`OK ~body:"nodrain" ()
    |_ -> 
        ignore @@ Aeio.async (fun () -> Aeio.sleep 0.1; exit 0) ();
        Server.respond_string ~status:`OK ~body:"shutting down" ()
  in
  let addr, port = Unix.inet_addr_loopback, 8081 in
  let saddr = Unix.ADDR_INET (addr, port) in
  let ssock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (* SO_REUSEADDR so we can restart the server quickly. *)
  Unix.setsockopt ssock Unix.SO_REUSEADDR true;
  Unix.bind ssock saddr;
  Unix.listen ssock 20;
  (* Socket is non-blocking *)
  Unix.set_nonblock ssock;
  let client_sock, client_addr = Aeio.accept ssock in
  let cn = string_of_sockaddr client_addr in
  printf "server : client (%s) connected.\n%!" cn;
  Unix.set_nonblock client_sock;
  Server.make ~callback ()

open Lwt
open Cohttp_lwt_unix

let not_none n t fn =
  t >>= function
  |None -> prerr_endline ("ERR None " ^ n); exit 1
  |Some x -> return (fn x) >>= fun () -> return (prerr_endline ("OK " ^ n))

let not_none_s n t fn =
  t >>= function
  |None -> prerr_endline ("ERR None " ^ n); exit 1
  |Some x -> fn x >>= fun () -> return (prerr_endline ("OK " ^ n))

let lwt_test_s t ~name ~assert_ =
  Lwt.catch (fun () ->
    t >>= assert_ >|= fun () -> prerr_endline ("OK " ^ name)
  ) (fun exn ->
    print_endline ("ERR " ^ name ^ " " ^ (Printexc.to_string exn));
    exit 1)

let lwt_test t ~name ~assert_ =
  lwt_test_s t ~name ~assert_:(fun a -> a |> assert_ |> return)

let rec do_times i f =
  if i = 0
  then return ()
  else
    f () >>= (fun () -> do_times (i - 1) f)

let client () =
  (* Do a set of single calls first and consume the body *)
  do_times 1000 (fun () ->
    lwt_test ~name:"get 1" (Client.get url) ~assert_:(fun (_,b) -> assert(b = `Empty))
    >>= fun () ->
    lwt_test_s ~name:"post 1" (Client.post ~body:(Cohttp_lwt_body.of_string "foobar") url)
     ~assert_:(fun (r,b) ->
       Cohttp_lwt_body.to_string b >>= fun _ ->
       lwt_test ~name:"get 1" (Client.get url) ~assert_:(fun (_,b) -> assert(b = `Empty))
       >>= fun () ->
       lwt_test_s ~name:"post 1" (Client.post ~body:(Cohttp_lwt_body.of_string "barfoo") url)
       ~assert_:(fun (r,b) ->
         Cohttp_lwt_body.to_string b >>= fun b ->
         assert (b = "barfoo");
         return ()
     ))
) >>= fun () ->
  (* Repeat but do not consume body *)
  do_times 2000 (fun () ->
  Lwt.catch (fun () ->
        lwt_test ~name:"get 1" (Client.get url) ~assert_:(fun (r,b) -> assert(b = `Empty))
        >>= fun () ->
        lwt_test_s ~name:"post 1" (Client.post ~body:(Cohttp_lwt_body.of_string "foobar") url)
          ~assert_:(fun (r,b) -> return ())
  ) (fun exn ->
        printf "got error, running gc\n%!";
        Gc.compact ();
        lwt_test ~name:"get 1" (Client.get url) ~assert_:(fun (r,b) -> assert(b = `Empty))
        >>= fun () ->
        lwt_test_s ~name:"post 1" (Client.post ~body:(Cohttp_lwt_body.of_string "foobar") url)
          ~assert_:(fun (r,b) -> return ()))
  ) >>= fun () ->
  (* Do a callv *)
  let body () = Cohttp_lwt_body.of_string "foobar" in
  let body1 = body () in
  let body2 = body () in
  let meth = `POST in
  let encoding = Transfer.Chunked in
  let reqs = Lwt_stream.of_list [
    Request.make ~meth ~encoding (Uri.of_string "/post"), body1;
    Request.make ~meth ~encoding ~headers:(Header.of_list ["connection","close"])
      (Uri.of_string "/post"), body2;
  ] in
  Client.callv url reqs >>= fun resp ->
  Lwt_stream.iter_s (fun (res, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
    assert(body="foobar");
    return ()
  ) resp >>= fun () ->
  Client.get url_shutdown >>= fun _ ->
  return (exit 1)

let _ =
  (* Fork into a client and server *)
  match Lwt_unix.fork () with
  |0 -> (* child / client *)
    Unix.sleep 2;
    Printf.eprintf "client is %d\n%!" (Unix.getpid ());
    Lwt_main.run (client ())
  |pid -> (* parent / server *)
    Printf.eprintf "server is %d\n%!" (Unix.getpid ());
    make_server ()
