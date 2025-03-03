open Lwt.Syntax
open Router
open Cohttp

let middlewares = ref []

let register_middleware (mw : middleware) =
  middlewares := mw :: !middlewares

let apply_middlewares (mws : middleware list) (final_h : handler) : handler =
  List.fold_right (fun mw acc -> mw acc) mws final_h

let callback_with_routing_and_middleware (req : Cohttp.Request.t) (body : Cohttp_lwt.Body.t) =
  let route_opt = Router.find_route req in
  match route_opt with
  | Some (route, params) ->
    let all_middlewares = List.rev !middlewares @ route.middlewares in
    let final_handler = route.handler in
    let wrapped_handler = apply_middlewares all_middlewares final_handler in
    wrapped_handler req body params None
  | None ->    
    Router.default_handler req body

let logger_middleware (next : handler) : handler =
  fun req body params session_data ->
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let meth = Cohttp.Request.meth req in
    let* _ = Lwt_io.printf "[logger] %s %s\n" (Cohttp.Code.string_of_method meth) path in    
    next req body params session_data

let secret_middleware (next : handler) : handler =
  fun req body params session_data ->
    let headers = Cohttp.Request.headers req in
    match Cohttp.Header.get headers "X-Secret" with
    | Some "abcdefg" ->      
      next req body params session_data
    | _ ->      
      let resp = Cohttp.Response.make ~status:`Forbidden () in
      let body = Cohttp_lwt.Body.of_string "Forbidden: missing or invalid X-Secret" in
      Lwt.return (resp, body)


 let session_middleware: Session.In_memory_store.t -> middleware = fun store next ->
    fun req body _params _  ->
    (* parse cookies from req *)
    let cookies = let headers = Request.headers req in
    match Header.get headers "Cookie" with
    | Some cookie_str -> 
        cookie_str 
        |> String.split_on_char ';' 
        |> List.map (fun s -> 
            match String.split_on_char '=' (String.trim s) with
            | [k; v] -> (String.trim k, String.trim v)
            | _ -> ("", ""))
        |> List.filter (fun (k, _) -> k <> "")
    | None -> []
    in
    let session_id_opt = List.assoc_opt "session_id" cookies in
    (* check if we have session in store *)    
    let* (sid, session_data) =
      match session_id_opt with
      | Some sid ->
          let* data_opt = Session.In_memory_store.get store sid in
          (match data_opt with
           | Some d ->               
               Lwt.return (sid, d)
           | None ->               
               Session.In_memory_store.create_session store (Hashtbl.create 100))
      | None ->
          Session.In_memory_store.create_session store (Hashtbl.create 100)
    in
       let* (resp, resp_body) = next req body _params (Some session_data) in

       let need_new_cookie =
         match session_id_opt with
         | Some existing_id when existing_id = sid -> false
         | _ -> true
       in
   
       let resp =
         if need_new_cookie then
           let headers = Response.headers resp in           
           let cookie_value =
             Printf.sprintf "session_id=%s; Path=/; HttpOnly" sid             
           in
           let headers = Header.add headers "Set-Cookie" cookie_value in
           Response.make ~status:(Response.status resp) ~headers ()
         else
           resp
       in
   
       Lwt.return (resp, resp_body)