open Alcotest
open Microcamel_web.Router

let test_handler : handler =
  fun _req _body _params _data -> 
    Lwt.return (Cohttp.Response.make ~status:`OK (), Cohttp_lwt.Body.empty)

let test_find_route _switch () =
  clear_routes ();
  
  add_route ~path:"/test" ~method_:`GET ~handler:test_handler ();
  
  let test_request = 
    Cohttp.Request.make 
      ~meth:`GET
      (Uri.of_string "/test")
  in
  
  match find_route test_request with
  | Some (route, _) -> 
      let () = check string "Route path matches" "/test" route.path in
      let () = check bool "Handler is present" true (route.handler == test_handler) in
      Lwt.return_unit
  | None -> 
      Alcotest.fail "Expected route '/test' was not found"

  
let test_missing_method _switch () = 
  clear_routes ();
  
  add_route ~path:"/test" ~method_:`GET ~handler:test_handler ();
  
  let test_request = 
    Cohttp.Request.make 
      ~meth:`POST
      (Uri.of_string "/test")
  in
  
  match find_route test_request with
  | Some _ -> 
      Alcotest.fail "Expected route '/test' was found"
  | None -> 
      Lwt.return_unit


let test_missing_route _switch () =
  clear_routes ();
  
  let test_request = 
    Cohttp.Request.make 
      ~meth:`GET
      (Uri.of_string "/nonexistent")
  in
  
  match find_route test_request with
  | Some (route, _) -> 
      Alcotest.fail (Printf.sprintf "Unexpectedly found route: %s" route.path)
  | None -> 
      Lwt.return_unit

let test_dynamic_route _switch () =
  clear_routes ();
  
  add_route ~path:"/users/:id" ~method_:`GET ~handler:test_handler ();
  
  let test_request = 
    Cohttp.Request.make 
      ~meth:`GET
      (Uri.of_string "/users/42")
  in
  
  match find_route test_request with
  | Some (route, params) -> 
      let () = check string "Route path matches" "/users/:id" route.path in
      let () = check string "Dynamic param matches" "42" (List.assoc "id" params) in
      Lwt.return_unit
  | None -> 
      Alcotest.fail "Expected dynamic route '/users/:id' was not found"

let test_combined_route _switch () =
  clear_routes ();
  
  add_route ~path:"/projects/:project_id/*" ~method_:`GET ~handler:test_handler ();
  
  let test_request = 
    Cohttp.Request.make 
      ~meth:`GET
      (Uri.of_string "/projects/123/files/code")
  in
  
  match find_route test_request with
  | Some (route, params) -> 
      let () = check string "Route path matches" "/projects/:project_id/*" route.path in
      let () = check string "Dynamic param matches" "123" (List.assoc "project_id" params) in
      let () = check string "Wildcard matches full path" "files/code" (List.assoc "*" params) in
      Lwt.return_unit
  | None -> 
      Alcotest.fail "Expected combined route '/projects/:project_id/*' was not found"



let () =
  let open Alcotest_lwt in
  Lwt_main.run @@ run "Router tests" [
    "route_finding", [
      test_case "find existing route" `Quick test_find_route;
      test_case "handle missing route" `Quick test_missing_route;
      test_case "handle missing method" `Quick test_missing_method;
      test_case "handle dynamic route" `Quick test_dynamic_route;
      test_case "handle combined route" `Quick test_combined_route;
    ];
  ]
