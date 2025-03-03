open Lwt.Infix
open Microcamel_web

module Store = Session.In_memory_store

(* Test 1: Sprawdzamy, że nowo utworzony store nie zawiera żadnej sesji 
   (czyli wywołanie get dla dowolnego SID zwraca None). *)
let test_store_empty_on_create _switch () =
  let lifetime = 10.0 in
  Store.create lifetime >>= fun store ->
  (* Spróbujemy 'get' na jakimś fikcyjnym sesyjnym ID. Oczekujemy None. *)
  Store.get store "nonexistent_session" >|= fun data_opt ->
  Alcotest.(check bool) "Store should have no session" false (Option.is_some data_opt)

(* Test 2: Tworzymy nową sesję i sprawdzamy, czy get zwraca Some. *)
let test_create_and_get_session _switch () =
  Store.create 10.0 >>= fun store ->
  let data = Hashtbl.create 5 in
  Hashtbl.replace data "foo" "bar";
  Store.create_session store data >>= fun (sid, _) ->
  (* Odczytujemy z get. Oczekujemy Some ... *)
  Store.get store sid >|= fun result_opt ->
  Alcotest.(check bool) "Should be Some data" true (Option.is_some result_opt);
  match result_opt with
  | None ->
      (* Nie powinniśmy tu trafić, ale Alcotest.fail uniemozliwia dalszy test. *)
      Alcotest.fail "Session was not found after create_session"
  | Some data_in_store ->
      let v = Hashtbl.find_opt data_in_store "foo" in
      Alcotest.(check (option string)) "key foo = bar" (Some "bar") v

(* Test 3: [set] i ponowny [get], żeby sprawdzić, czy klucz się zapisał. *)
let test_set_session_value _switch () =
  Store.create 10.0 >>= fun store ->
  let data = Hashtbl.create 5 in
  (* Na starcie w data nie ma klucza 'counter'. *)
  Store.create_session store data >>= fun (sid, _) ->
  Store.set store sid "counter" "42" >>= fun () ->
  (* Teraz odczytujemy i sprawdzamy, czy 'counter' == "42". *)
  Store.get store sid >|= fun data_opt ->
  match data_opt with
  | None ->
      Alcotest.fail "Session shouldn't be None after set"
  | Some session_data ->
      let c = Hashtbl.find_opt session_data "counter" in
      Alcotest.(check (option string)) "counter == 42" (Some "42") c

(* Test 4: [remove] – usuwamy sesję i oczekujemy, że get zwróci None. *)
let test_remove_session _switch () =
  Store.create 10.0 >>= fun store ->
  let data = Hashtbl.create 5 in
  Store.create_session store data >>= fun (sid, _) ->
  (* Najpierw sprawdzamy, że get zwraca Some. *)
  Store.get store sid >>= fun before_opt ->
  Alcotest.(check bool) "Should have session" true (Option.is_some before_opt);

  (* Usuwamy. *)
  Store.remove store sid >>= fun () ->
  (* Sprawdzamy, że teraz get zwraca None. *)
  Store.get store sid >|= fun after_opt ->
  Alcotest.(check bool) "Should be None after remove" false (Option.is_some after_opt)

let () =
  let open Alcotest_lwt in
  Lwt_main.run @@ run "Session Store tests" [
    "Store basic", [
      test_case "store is empty on create"  `Quick test_store_empty_on_create;
      test_case "create and get session"    `Quick test_create_and_get_session;
      test_case "set session value"         `Quick test_set_session_value;
      test_case "remove session"            `Quick test_remove_session;
    ]    
  ]