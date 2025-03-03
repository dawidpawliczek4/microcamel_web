open Lwt.Syntax
open Microcamel_web.Middleware
open Microcamel_web.Router

let setup_test_env () =
  clear_routes ();
  middlewares := [];
  (* Rejestrujemy nasz secret_middleware, aby działał globalnie. *)
  register_middleware secret_middleware;
  (* Dodajmy prostą trasę GET /hello *)
  add_route
    ~path:"/hello"
    ~method_:`GET
    ~handler:(fun _req _body _params _data->
       let resp = Cohttp.Response.make ~status:`OK () in
       let body = Cohttp_lwt.Body.of_string "Hello from /hello" in
       Lwt.return (resp, body)
    ) ();
  (* Zwracamy callback, który stosuje routing + middlewares. *)
  callback_with_routing_and_middleware

(* Test 1: Poprawny nagłówek "X-Secret: abracadabra" -> spodziewamy się 200 OK *)
let test_secret_middleware_valid () =
  let callback = setup_test_env () in
  (* Przygotowujemy sztuczne żądanie: GET /hello z właściwym "X-Secret" *)
  let headers = Cohttp.Header.of_list [("X-Secret","abcdefg")] in
  let req = Cohttp.Request.make ~headers (Uri.of_string "/hello") in
  let body = Cohttp_lwt.Body.empty in

  (* Wołamy nasz callback bezpośrednio. *)
  let* (resp, _resp_body) = callback req body in  
    let status = Cohttp.Response.status resp in
    let code = Cohttp.Code.code_of_status status in

    (* Sprawdzamy, czy status to 200 *)
    Alcotest.(check int) "Expected 200 OK" 200 code;

    Lwt.return_unit

(* Test 2: Brak/niepoprawny nagłówek "X-Secret" -> spodziewamy się 403 Forbidden *)
let test_secret_middleware_invalid () =
  let callback = setup_test_env () in
  (* Tym razem w ogóle nie dajemy X-Secret, albo dajemy inny. *)
  let headers = Cohttp.Header.of_list [("X-Secret","invalid")] in
  let req = Cohttp.Request.make ~headers (Uri.of_string "/hello") in
  let body = Cohttp_lwt.Body.empty in

  let* (resp, _resp_body) = callback req body in   
    let status = Cohttp.Response.status resp in
    let code = Cohttp.Code.code_of_status status in

    Alcotest.(check int) "Expected 403 Forbidden" 403 code;
    Lwt.return_unit

(* Entry point: odpalamy Alcotest *)
let () =
  let open Alcotest_lwt in
  Lwt_main.run @@ run "middleware" [
    "middleware", [
      test_case "secret_middleware valid" `Quick (fun _switch () ->
        test_secret_middleware_valid ()
      );
      test_case "secret_middleware invalid" `Quick (fun _switch () ->
        test_secret_middleware_invalid ()
      );
    ];
  ]