open Lwt.Syntax
open Router
open Cohttp

let middlewares = ref []

let register_middleware (mw : middleware) =
  middlewares := mw :: !middlewares

let apply_middlewares (mws : middleware list) (final_h : handler) : handler =
  List.fold_right (fun mw acc -> mw acc) mws final_h

(* glowna funkcja *)
(* To będzie callback, który podamy do [start_server], 
  zamiast poprzedniego „gołego” callbacka. *)
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


(* -------------------------------------------------- *)
(* PRZYKŁADOWE MIDDLEWARES                            *)
(* -------------------------------------------------- *)

(* 1) Proste middleware logujące metodę i ścieżkę *)
let logger_middleware (next : handler) : handler =
  fun req body params session_data ->
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let meth = Cohttp.Request.meth req in
    let* _ = Lwt_io.printf "[logger] %s %s\n" (Cohttp.Code.string_of_method meth) path in
    (* Wywołujemy właściwy (następny) handler *)
    next req body params session_data

(* 2) Middleware, które sprawdza nagłówek "X-Secret" i 
      jeśli nie ma poprawnej wartości, zwraca 403 Forbidden *)
let secret_middleware (next : handler) : handler =
  fun req body params session_data ->
    let headers = Cohttp.Request.headers req in
    match Cohttp.Header.get headers "X-Secret" with
    | Some "abracadabra" ->
      (* OK, przechodzimy dalej *)
      next req body params session_data
    | _ ->
      (* Blokujemy i zwracamy błąd 403 *)
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
               (* Istnieje ważna sesja z tym ID *)
               Lwt.return (sid, d)
           | None ->
               (* Nie ma takiej sesji (wygasła/usunięta) -> tworzymy nową *)
               Session.In_memory_store.create_session store (Hashtbl.create 100))
      | None ->
          (* Nie było ciasteczka -> tworzymy nową sesję *)
          Session.In_memory_store.create_session store (Hashtbl.create 100)
    in

    (* let session_deleted = ref false in
    let delete_session () = session_deleted := true in
    (* Add session to request context *)
    let req = add_session_to_request req session_data delete_session in
    let* resp = handler req in
    let resp =
      if !session_deleted then
        let%lwt () = In_memory_store.remove store new_id in
        add_cookie resp "session_id=; Expires=Thu, 01 Jan 1970 00:00:00 GMT"
      else if new_id <> original_id then
        add_cookie resp (make_cookie new_id)
      else resp
    in
    Lwt.return resp *)

    (* 3. Wywołujemy kolejny handler w łańcuchu, przekazując mu `Some session_data`.
       Ignorujemy tutaj ewentualne `session_opt`, bo i tak mamy świeże info 
       w store / w cookies. Ale gdybyś chciał chainować kilka session_middleware
       w jednej ścieżce, możesz to scalić według potrzeb. *)
       let* (resp, resp_body) = next req body _params (Some session_data) in

       (* 4. Jeśli stary ID (z cookies) był inny niż faktyczny `sid`, to znaczy,
             że mamy nową sesję -> ustawiamy ciasteczko w odpowiedzi. 
          Możesz też zrezygnować z warunku i *zawsze* ustawiać session_id,
          co często jest prostszą praktyką. *)
   
       let need_new_cookie =
         match session_id_opt with
         | Some existing_id when existing_id = sid -> false
         | _ -> true
       in
   
       let resp =
         if need_new_cookie then
           let headers = Response.headers resp in
           (* TUTAJ budujemy linijkę Set-Cookie = "session_id=XYZ; ..." *)
           let cookie_value =
             Printf.sprintf "session_id=%s; Path=/; HttpOnly" sid
             (* Możesz dopisać "; Secure" czy "SameSite=Strict" *)
           in
           let headers = Header.add headers "Set-Cookie" cookie_value in
           Response.make ~status:(Response.status resp) ~headers ()
         else
           resp
       in
   
       Lwt.return (resp, resp_body)