open Lwt.Syntax

(* session_store.ml *)
module type SESSION_STORE = sig
  type t
  type session_id = string
  type session_data = (string, string) Hashtbl.t

  val create : float -> t Lwt.t
  val get : t -> session_id -> session_data option Lwt.t
  val create_session : t -> session_data -> session_id Lwt.t
  val remove : t -> session_id -> unit Lwt.t
end

module In_memory_store : SESSION_STORE = struct
  type session_id = string
  type session_data = (string, string) Hashtbl.t
  type session_entry = { data : session_data; expires_at : float }
  type t = {
    sessions : (session_id, session_entry) Hashtbl.t;
    mutex : Lwt_mutex.t;
    lifetime : float;
  }

  let create lifetime =
    let store = { sessions = Hashtbl.create 100; mutex = Lwt_mutex.create (); lifetime } in
    (* Background cleanup every 5 minutes *)
    let rec cleanup () =
      let* () = Lwt_unix.sleep 300.0 in
      let now = Unix.gettimeofday () in
      let* () = Lwt_mutex.with_lock store.mutex (fun () ->
        Hashtbl.filter_map_inplace (fun _ entry ->
          if entry.expires_at > now then Some entry else None) store.sessions;
        Lwt.return ()
        ) 
      in
      cleanup ()
    in
    Lwt.async cleanup;
    Lwt.return store

  let get store id =
    Lwt_mutex.with_lock store.mutex (fun () ->
      let now = Unix.gettimeofday () in
      match Hashtbl.find_opt store.sessions id with
      | Some entry when entry.expires_at > now ->
          let new_entry = { entry with expires_at = now +. store.lifetime } in
          Hashtbl.replace store.sessions id new_entry;
          Lwt.return (Some entry.data)
      | Some _ | None ->
          Hashtbl.remove store.sessions id;
          Lwt.return None)

  let generate_session_id () =
    let random_char () =
      let chars = "abcdef0123456789" in
      let len = String.length chars in
      chars.[Random.int len]
    in
    let rec generate acc n =
      if n <= 0 then acc
      else generate (random_char () :: acc) (n - 1)
    in
    let id = String.of_seq (List.to_seq (generate [] 64)) in (* 64-character ID *)
    id

  let create_session store data =
    let id = generate_session_id () in (* Implement secure ID generation *)
    let now = Unix.gettimeofday () in
    let entry = { data; expires_at = now +. store.lifetime } in
    Lwt_mutex.with_lock store.mutex (fun () ->
      Hashtbl.add store.sessions id entry;
      Lwt.return id)

  let remove store id =
    Lwt_mutex.with_lock store.mutex (fun () ->
      Hashtbl.remove store.sessions id;
      Lwt.return ())      
end