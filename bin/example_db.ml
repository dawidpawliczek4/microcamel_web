open Microcamel_web
open Cohttp
open Cohttp_lwt_unix

type user_row = {
  id: int64;
  username: string;
  email: string;
  created_at: string;
}

let map_user_row row = 
  match row with
  | [| Sqlite3.Data.INT id; Sqlite3.Data.TEXT username; Sqlite3.Data.TEXT email; Sqlite3.Data.TEXT created_at; |] -> {id; username; email; created_at}
  | _ -> failwith "Invalid row"

let users_handler _req _body _params _data =
  let result = 
    Database.query (Database.Mapper map_user_row) "SELECT id, username, email, created_at FROM users"
  in
  
  (* Convert to JSON *)
  let json = 
    let users = List.map (fun row ->      
      Printf.sprintf {|{
        "id": %s,
        "username": "%s",
        "email": "%s",
        "created_at": "%s"
      }|} 
      (Int64.to_string row.id) row.username row.email row.created_at
    ) result in
    Printf.sprintf "[%s]" (String.concat "," users)
  in
  
  let response = 
    Response.make 
      ~status:`OK 
      ~headers:(Header.init_with "Content-Type" "application/json")
      ()
  in
  Lwt.return (response, Cohttp_lwt.Body.of_string json)

(* Handler for getting single user *)
let user_handler _req _body _params _data =  
  let param = List.assoc "id" _params in

  let result = 
    Database.query (Database.Mapper map_user_row)
      ("SELECT id, username, email, created_at FROM users WHERE id = " ^ param )      
  in
  
  match result with
  | [] ->
      let response = 
        Response.make 
          ~status:`Not_found 
          ~headers:(Header.init_with "Content-Type" "application/json")
          ()
      in
      Lwt.return (response, Cohttp_lwt.Body.of_string "{\"error\": \"User not found\"}")
  | row::_ ->    
      let json = Printf.sprintf {|{
        "id": %s,
        "username": "%s",
        "email": "%s",
        "created_at": "%s"
      }|}
      (Int64.to_string row.id) row.username row.email row.created_at
      in
      let response = 
        Response.make 
          ~status:`OK 
          ~headers:(Header.init_with "Content-Type" "application/json")
          ()
      in
      Lwt.return (response, Cohttp_lwt.Body.of_string json)

let () =
  Database.init_database ~db_file:"example.db" ~init_sql_file:"init.sql";

  Router.add_route ~path:"/users" ~method_:`GET ~handler:users_handler ();
  Router.add_route ~path:"/users/:id" ~method_:`GET ~handler:user_handler ();
  
  Lwt_main.run (Microcamel_web.Server.start_server ~port:8080)
