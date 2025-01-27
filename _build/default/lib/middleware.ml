open Lwt.Syntax
open Router

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
