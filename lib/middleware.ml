open Lwt.Syntax
open Router
open Cohttp

let middlewares = ref []

let register_middleware (mw : middleware) =
  middlewares := mw :: !middlewares

(* Funkcja pomocnicza, która „składa” łańcuch middleware.
   - Robimy fold_right, żeby zachować kolejność nakładania:
     jeśli mamy [mw1; mw2], to mw1 jest „bliżej” użytkownika, 
     a mw2 jest „bliżej” finalnego handlera. *)
let apply_middlewares (mws : middleware list) (final_h : handler) : handler =
  List.fold_right (fun mw acc -> mw acc) mws final_h


  (* glowna funkcja *)
  (* To będzie callback, który podamy do [start_server], 
   zamiast poprzedniego „gołego” callbacka. *)
let callback_with_routing_and_middleware (req : Cohttp.Request.t) (body : Cohttp_lwt.Body.t) =
  (* Znajdźmy najpierw pasujący route. Jeśli go nie ma -> default_handler *)
  let route_opt = Router.find_route req in
  match route_opt with
  | Some (route, params) ->
    (* Bierzemy właściwy handler, nakładamy na niego *wszystkie* middleware,
       a dopiero potem wywołujemy z (req, body, params). *)
    let all_middlewares = List.rev !middlewares @ route.middlewares in
    let final_handler = route.handler in
    let wrapped_handler = apply_middlewares all_middlewares final_handler in
    wrapped_handler req body params
  | None ->
    (* Brak pasującego routa -> 404 *)
    Router.default_handler req body



  (* -------------------------------------------------- *)
(* PRZYKŁADOWE MIDDLEWARES                            *)
(* -------------------------------------------------- *)

(* 1) Proste middleware logujące metodę i ścieżkę *)
let logger_middleware (next : handler) : handler =
  fun req body params ->
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let meth = Cohttp.Request.meth req in
    let* _ = Lwt_io.printf "[logger] %s %s\n" (Cohttp.Code.string_of_method meth) path in
    (* Wywołujemy właściwy (następny) handler *)
    next req body params

(* 2) Middleware, które sprawdza nagłówek "X-Secret" i 
      jeśli nie ma poprawnej wartości, zwraca 403 Forbidden *)
let secret_middleware (next : handler) : handler =
  fun req body params ->
    let headers = Cohttp.Request.headers req in
    match Cohttp.Header.get headers "X-Secret" with
    | Some "abracadabra" ->
      (* OK, przechodzimy dalej *)
      next req body params
    | _ ->
      (* Blokujemy i zwracamy błąd 403 *)
      let resp = Cohttp.Response.make ~status:`Forbidden () in
      let body = Cohttp_lwt.Body.of_string "Forbidden: missing or invalid X-Secret" in
      Lwt.return (resp, body)

(* session_middleware.ml *)
let session_middleware handler =
  fun req body params->
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
    let original_id = List.assoc_opt "session_id" cookies in
    let%lwt (new_id, session_data) =
      match original_id with
      | Some id ->
          let%lwt data = In_memory_store.get store id in
          (match data with
          | Some d -> Lwt.return (id, d)
          | None -> create_new_session store)
      | None -> create_new_session store
    in
    let session_deleted = ref false in
    let delete_session () = session_deleted := true in
    (* Add session to request context *)
    let req = add_session_to_request req session_data delete_session in
    let%lwt resp = handler req in
    let resp =
      if !session_deleted then
        let%lwt () = In_memory_store.remove store new_id in
        add_cookie resp "session_id=; Expires=Thu, 01 Jan 1970 00:00:00 GMT"
      else if new_id <> original_id then
        add_cookie resp (make_cookie new_id)
      else resp
    in
    Lwt.return resp