open Microcamel_web.Server
open Microcamel_web.Middleware
open Microcamel_web.Router
open Microcamel_web
open Cohttp

let session_store =
  Lwt_main.run (Session.In_memory_store.create 600.0)


let count_handler: handler =
 fun _ _ _ session_data_opt ->
  match session_data_opt with
  | None ->      let resp = Response.make ~status:`Internal_server_error () in
  let body = Cohttp_lwt.Body.of_string "No session available" in
  Lwt.return (resp, body)
  | Some data ->
    let current =
    match Hashtbl.find_opt data "count" with
    | Some c -> int_of_string c
    | None -> 0
  in
  let new_count = current + 1 in
  
  Hashtbl.replace data "count" (string_of_int new_count);

  let msg = Printf.sprintf "Count in session = %d" new_count in
  let resp = Response.make ~status:`OK () in
  let body = Cohttp_lwt.Body.of_string msg in
  Lwt.return (resp, body)


let () = 
  add_route 
    ~path:"/hello" 
    ~method_:`GET 
    ~handler:count_handler ();

  register_middleware (session_middleware session_store);

  Lwt_main.run (start_server ~port:8080);