open Microcamel_web.Template
open Printf

let () =
  let template_str = {|
Hello, {{ user }}!
{{#if is_admin}}
  <p>You are an admin!</p>
{{#else}}
  <p>You are a regular user.</p>
{{/if}}

<ul>
{{#for item in items}}
  <li>{{ item }}</li>
{{/for}}
</ul>
|} in

  (* Tworzymy kontekst i wstawiamy przykładowe dane *)
  let ctx : context = Hashtbl.create 10 in
  Hashtbl.add ctx "user" (Str "Alice");
  Hashtbl.add ctx "is_admin" (Bool false);
  Hashtbl.add ctx "items" (List [Str "Item1"; Str "Item2"; Str "Item3"]);

  let rendered = render_template ctx template_str in
  printf "Wygenerowany HTML:\n%s\n" rendered
