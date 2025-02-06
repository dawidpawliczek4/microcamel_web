open Str

(* ===================== *)
(* DEFINICJE TYPÓW *)
(* ===================== *)

(* Typ wartości, które możemy wstawić do szablonu *)
type value =
  | Str of string
  | Bool of bool
  | List of value list

(* Kontekst – mapa nazw -> value *)
type context = (string, value) Hashtbl.t

(* AST szablonu *)
type node =
  | Text of string                      (* Zwykły tekst *)
  | Var of string                       (* {{ var }} *)
  | IfBlock of string * node list * node list  (* {{#if cond}} ... {{#else}} ... {{/if}} *)
  | ForBlock of string * string * node list     (* {{#for item in list}} ... {{/for}} *)

(* Tokeny uzyskane z szablonu *)
type token =
  | T_Text of string
  | T_Var of string
  | T_If of string        (* np. "{{#if cond}}" *)
  | T_Else                (* "{{#else}}" *)
  | T_EndIf               (* "{{/if}}" *)
  | T_For of string * string   (* np. "{{#for item in list}}" *)
  | T_EndFor              (* "{{/for}}" *)

(* ===================== *)
(* POMOCNICZE FUNKCJE *)
(* ===================== *)

(* Używamy String.trim – ewentualnie definiujemy alias *)
let trim = String.trim

(* Funkcja extrahująca zawartość z tekstu postaci "{{ ... }}" *)
let extract_tag s =
  let re = regexp "{{\\(.*?\\)}}" in
  if string_match re s 0 then
    matched_group 1 s |> trim
  else
    s |> trim

(* ===================== *)
(* TOKENIZATOR *)
(* ===================== *)

let tokenize (template : string) : token list =
  (* Szukamy fragmentów {{ ... }} *)
  let re = regexp "{{\\(.*?\\)}}" in
  let parts = full_split re template in
  List.map (fun part ->
    match part with
    | Str.Text s -> T_Text s
    | Str.Delim s ->
        let tag = extract_tag s in
        if string_match (regexp "#if[ \t]+\\(.*\\)") tag 0 then
          let cond = matched_group 1 tag |> trim in
          T_If cond
        else if tag = "#else" then
          T_Else
        else if tag = "/if" then
          T_EndIf
        else if string_match (regexp "#for[ \t]+\\([^ \t]+\\)[ \t]+in[ \t]+\\(.*\\)") tag 0 then
          let item = matched_group 1 tag |> trim in
          let list_name = matched_group 2 tag |> trim in
          T_For (item, list_name)
        else if tag = "/for" then
          T_EndFor
        else
          T_Var tag
  ) parts

(* ===================== *)
(* PARSER *)
(* ===================== *)

let parse_tokens (tokens : token list) : (node list * token list) =
  let rec aux acc tokens =
    match tokens with
    | [] -> (List.rev acc, [])
    | T_Text s :: rest -> aux (Text s :: acc) rest
    | T_Var v :: rest -> aux (Var v :: acc) rest
    | T_If cond :: rest ->
        let (if_nodes, rem) = aux [] rest in
        let (else_nodes, rem2) =
          match rem with
          | T_Else :: rest2 ->
              let (els, rem3) = aux [] rest2 in
              (els, rem3)
          | _ -> ([], rem)
        in
        let rem3 =
          match rem2 with
          | T_EndIf :: rest3 -> rest3
          | _ -> rem2
        in
        aux (IfBlock (cond, if_nodes, else_nodes) :: acc) rem3
    | T_Else :: _ -> (List.rev acc, tokens)
    | T_EndIf :: _ -> (List.rev acc, tokens)
    | T_For (item, list_name) :: rest ->
        let (for_nodes, rem) = aux [] rest in
        let rem2 =
          match rem with
          | T_EndFor :: rest2 -> rest2
          | _ -> rem
        in
        aux (ForBlock (item, list_name, for_nodes) :: acc) rem2
    | T_EndFor :: _ -> (List.rev acc, tokens)
  in
  aux [] tokens

let parse_template (template : string) : node list =
  let tokens = tokenize template in
  let (nodes, _) = parse_tokens tokens in
  nodes

(* ===================== *)
(* RENDERER *)
(* ===================== *)

let rec render_node (ctx : context) (n : node) : string =
  match n with
  | Text s -> s
  | Var v ->
      (match Hashtbl.find_opt ctx v with
       | Some (Str s) -> s
       | Some (Bool b) -> if b then "true" else "false"
       | Some (List _) -> "<list>"  (* Nie interpolujemy list bezpośrednio *)
       | None -> "")
  | IfBlock (cond, then_nodes, else_nodes) ->
      (match Hashtbl.find_opt ctx cond with
       | Some (Bool true) -> render_list ctx then_nodes
       | Some (Bool false) -> render_list ctx else_nodes
       | _ -> render_list ctx else_nodes)
  | ForBlock (item, list_name, nodes) ->
      (match Hashtbl.find_opt ctx list_name with
       | Some (List lst) ->
           let buffer = Buffer.create 128 in
           let old_binding = Hashtbl.find_opt ctx item in
           List.iter (fun v ->
             Hashtbl.replace ctx item v;
             Buffer.add_string buffer (render_list ctx nodes)
           ) lst;
           (match old_binding with
            | Some v -> Hashtbl.replace ctx item v
            | None -> Hashtbl.remove ctx item);
           Buffer.contents buffer
       | _ -> "")
and render_list (ctx : context) (nodes : node list) : string =
  String.concat "" (List.map (render_node ctx) nodes)

let render_template (ctx : context) (template : string) : string =
  let nodes = parse_template template in
  render_list ctx nodes