open Lwt.Syntax

let serve_file base_dir parsed_path =
  let full_path = (base_dir ^ "/" ^ parsed_path) in  
  if Sys.file_exists full_path && not (Sys.is_directory full_path) then
    let mime_type =
      match Filename.extension full_path with
      | ".html" -> "text/html"
      | ".css" -> "text/css"
      | ".js" -> "application/javascript"
      | ".png" -> "image/png"
      | ".jpg" | ".jpeg" -> "image/jpeg"
      | ".gif" -> "image/gif"
      | _ -> "application/octet-stream"
    in
    let* content = Lwt_io.with_file ~mode:Lwt_io.input full_path Lwt_io.read in    
    let headers = Cohttp.Header.init_with "Content-Type" mime_type in
    Lwt.return (Cohttp.Response.make ~status:`OK ~headers (), Cohttp_lwt.Body.of_string content)
  else
    let body = Cohttp_lwt.Body.of_string full_path in
    Lwt.return (Cohttp.Response.make ~status:`Not_found (), body)