open Lwt.Syntax
open Cohttp_lwt_unix

type server_error =
  | PortInUse
  | InvalidPort
  | NetworkError of string

let create_server ~port callback =
  if port <= 0 || port > 65535 then
    Lwt.return (Error InvalidPort)
  else
    let server_callback _conn req body =      
      Lwt.catch
        (fun () -> 
          let* (resp, body) = callback req body in
          Lwt.return (resp, body))
        (fun exn ->
          let error_response = 
            Response.make 
              ~status:`Internal_server_error 
              ~headers:(Cohttp.Header.init_with "Content-Type" "text/plain")
              ()
          in
          let error_message = Printf.sprintf "Server Error: %s" (Printexc.to_string exn) in
          let error_body = 
            Cohttp_lwt.Body.of_string error_message              
          in
          Lwt.return (error_response, error_body))
    in    
    Lwt.catch
      (fun () ->
        let* server = 
          Server.create 
            ~mode:(`TCP (`Port port)) 
            (Server.make ~callback:server_callback ())
        in
        Lwt.return (Ok server))
      (fun exn ->
        match exn with
        | Unix.Unix_error(Unix.EADDRINUSE, _, _) ->
            Lwt.return (Error PortInUse)
        | e ->  
            Lwt.return (Error (NetworkError (Printexc.to_string e))))

let start_server ~port=
  let* () = Lwt_io.printlf "Starting server on port %d..." port in
  let* result = create_server ~port Middleware.callback_with_routing_and_middleware in
  match result with
  | Ok _server ->
      let* () = Lwt_io.printlf "Server successfully started on port %d" port in
      let* () = Lwt_io.flush Lwt_io.stdout in
      let forever = Lwt_unix.sleep infinity in
      forever
  | Error PortInUse ->
      Lwt_io.printlf "Error: Port %d is already in use" port
  | Error InvalidPort ->
      Lwt_io.printlf "Error: Invalid port number %d" port
  | Error (NetworkError msg) ->
      Lwt_io.printlf "Error starting server: %s" msg
