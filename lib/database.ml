open Sqlite3

type ('row, 'r) query_mode =
  | Raw : (Data.t array, Data.t array list) query_mode      
  | Mapper : (Data.t array -> 'a) -> (Data.t array, 'a list) query_mode      

let db = ref None

let connect path = 
  match !db with
  | Some _ -> ()
  | None -> db := Some (db_open path)

let get_db () = 
  match !db with
  | Some db -> db
  | None -> failwith "Database not connected"


let query : type row r. (row, r) query_mode -> string -> r = 
  fun mode sql ->    
  let db = get_db () in
  let stmt = prepare db sql in
  let columns = Array.init (column_count stmt) (column_name stmt) in
  
  let rec fetch acc =
    match step stmt with
    | Rc.ROW ->       
      let row = Array.init (Array.length columns) (column stmt) in
      fetch (row :: acc)
    | Rc.DONE -> List.rev acc
    | err -> failwith ("Query failed: " ^ Rc.to_string err)
  in
  let rows = fetch [] in
  match mode with
  | Raw -> rows
  | Mapper mapper -> List.map mapper rows


let init_database ~db_file ~init_sql_file =  
  connect db_file;
    
  let init_sql = 
    let chan = open_in init_sql_file in
    let content = really_input_string chan (in_channel_length chan) in
    close_in chan;
    content
  in
  
  String.split_on_char ';' init_sql
  |> List.filter (fun sql -> String.trim sql <> "")
  |> List.iter (fun sql -> 
    let _ = query Raw sql in
    Printf.printf "Executed SQL: %s\n" (String.trim sql)
  )