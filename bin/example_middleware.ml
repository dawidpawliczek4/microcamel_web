open Microcamel_web.Server
open Microcamel_web.Middleware
open Microcamel_web.Router

let () =  
  add_route 
    ~path:"/hello" 
    ~method_:`GET 
    ~middlewares:[logger_middleware]
    ~handler:(fun _req _body _params _data ->
      let resp = Cohttp.Response.make ~status:`OK () in
      let body = Cohttp_lwt.Body.of_string "Hello World!" in
      Lwt.return (resp, body)
    ) ();
  
  add_route
    ~path:"/users/:id"
    ~method_:`GET
    ~middlewares:[logger_middleware]
    ~handler:(fun _req _body params _data ->
      match List.assoc_opt "id" params with
      | Some user_id ->
        let resp = Cohttp.Response.make ~status:`OK () in
        let body = Cohttp_lwt.Body.of_string (Printf.sprintf "User id: %s" user_id) in
        Lwt.return (resp, body)
      | None ->
        let resp = Cohttp.Response.make ~status:`Bad_request () in
        let body = Cohttp_lwt.Body.of_string "Missing user id param" in
        Lwt.return (resp, body)
    ) ();

  (* register_middleware logger_middleware;   *)

  Lwt_main.run (start_server ~port:8080);