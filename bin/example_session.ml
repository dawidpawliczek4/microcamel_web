open Lwt.Syntax
open Cohttp_lwt_unix
open Microcamel_web

(* Example handler that uses sessions *)
let handle_home: Router.handler = fun req _body _params ->
  let session = Session.In_memory_store.get_session req in
  let* visits =
    match Session.get session "visits" with
    | Some v -> Lwt.return (int_of_string v)
    | None -> Lwt.return 0
  in
  let* () = Session.In_memory_store.set session "visits" (string_of_int (visits + 1)) in
  let response_body = Printf.sprintf "Welcome! You have visited this page %d times.\n" (visits + 1) in
  let response = Response.make ~status:`OK () in
  Lwt.return (response, Cohttp_lwt.Body.of_string response_body)

(* Example handler to clear the session *)
let handle_clear req _body _params =
  let session = Session_middleware.get_session req in
  let* () = Session_store.In_memory_store.clear session in
  let response = Response.make ~status:`OK () in
  Lwt.return (response, Cohttp_lwt.Body.of_string "Session cleared.\n")

(* Example handler to show session data *)
let handle_show req _body _params =
  let session = Session_middleware.get_session req in
  let* data = Session_store.In_memory_store.get_all session in
  let response_body =
    data
    |> List.map (fun (k, v) -> Printf.sprintf "%s: %s\n" k v)
    |> String.concat ""
  in
  let response = Response.make ~status:`OK () in
  Lwt.return (response, Cohttp_lwt.Body.of_string response_body)

(* Main application setup *)
let main () =
  (* Initialize the session store with a 1-hour lifetime *)
  let* store = Session_store.In_memory_store.create 3600.0 in

  (* Define routes *)
  let router = Router.router [
    Router.get "/" handle_home;
    Router.get "/clear" handle_clear;
    Router.get "/show" handle_show;
  ] in

  (* Wrap the router with session middleware *)
  let app = Session_middleware.session_middleware store router in

  (* Start the server *)
  Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback:app ())

(* Run the application *)
let () =
  Lwt_main.run (main ())