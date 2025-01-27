open Cohttp_lwt_unix

type type_method = Cohttp.Code.meth

type handler = Request.t -> Cohttp_lwt.Body.t -> (string * string) list -> (Response.t * Cohttp_lwt.Body.t) Lwt.t

type middleware = handler -> handler
type route = { path: string; method_: type_method; handler: handler; middlewares: middleware list}

let routes = ref []

let clear_routes () = routes := []

let add_route ?(middlewares=[]) ~path ~method_ ~handler () =
  routes := { path; method_; handler; middlewares } :: !routes

(* function which takes path , takes some path from all routes, and return Some path and string * string list if matches, where string list is a list of params, or none *) 
  let parse_path (path: string) (route_path: string) : (string * string) list option =
    let path_parts = String.split_on_char '/' path |> List.filter ((<>) "") in
    let route_parts = String.split_on_char '/' route_path |> List.filter ((<>) "") in
    let rec aux path_parts route_parts acc =
      match path_parts, route_parts with
      | [], [] -> Some (List.rev acc)  (* Exact match *)
      | [], _ | _, [] -> None         (* Length mismatch *)
      | p :: ps, "*" :: _ ->
        (* Wildcard matches the remaining path *)
        let wildcard_path = String.concat "/" (p :: ps) in
        Some (List.rev (("*", wildcard_path) :: acc))        
      | p :: ps, r :: rs when String.length r > 0 && r.[0] = ':' ->
        (* Dynamic segment *)
        let key = String.sub r 1 (String.length r - 1) in
        aux ps rs ((key, p) :: acc)
      | p :: ps, r :: rs when p = r ->
          (* Static segment matches *)
          aux ps rs acc
      | _ -> None  (* Mismatch *)
    in
    aux path_parts route_parts []

let find_route req : (route * (string*string) list) option =
  let uri = Cohttp.Request.uri req in
  let method_ = Cohttp.Request.meth req in
  let path = Uri.path uri in  
  List.find_map
    (fun route ->
      if route.method_ = method_ then
        match parse_path path route.path with
        | Some params -> Some (route, params)
        | None -> None
      else
        None)
    !routes

let default_handler _req _body =
  let body = Cohttp_lwt.Body.of_string "404 Not Found" in
  Lwt.return (Cohttp.Response.make ~status:`Not_found (), body)

let use_static ~path ~base_dir =  
  let handler _ _body params =
    match params with
    | [(_, file_path)] -> Static.serve_file base_dir file_path
    | _ ->
        let body = Cohttp_lwt.Body.of_string "Invalid path" in
        Lwt.return (Cohttp.Response.make ~status:`Bad_request (), body)
    in
  add_route ~path ~method_:`GET ~handler ();