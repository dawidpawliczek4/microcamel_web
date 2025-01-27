open Microcamel_web.Server
open Microcamel_web.Router

let home_handler: handler = fun _req _body _params ->
  let body = Cohttp_lwt.Body.of_string "Hello, world!" in
  Lwt.return (Cohttp.Response.make ~status:`OK (), body)

let dynamic_handler: handler = fun _req _body _params ->
  let param = List.assoc "id" _params in
  let body = Cohttp_lwt.Body.of_string ("Hello, world, from " ^ param) in  
  Lwt.return (Cohttp.Response.make ~status:`OK (), body)

let () =
  add_route ~path:"/" ~method_:`GET ~handler:home_handler ();
  add_route ~path:"/:id" ~method_:`GET ~handler:dynamic_handler ();
  use_static ~path:"/static/*" ~base_dir:"./public";

  Lwt_main.run (start_server ~port:8080)
