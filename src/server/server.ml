open StdLabels

module Pages = struct
  open Ppx_deriving_router_runtime.Primitives

  type t =
    | Home [@GET "/"]
    | About of { active : bool }
    | Echo of { test : bool; nb : int }
    | Hello of { name : string; repeat : int option } [@GET "/hello/:name"]
  [@@deriving router]
end

let _hello = Pages.href (Hello { name = "corentin"; repeat = Some 2 })
let _echo = Pages.href (Echo { test = true; nb = 4 })

let () =
  print_newline ();
  print_endline (Pages.href (About { active = false }))

module Handler = struct
  let pages_handle =
    Pages.handle (fun route _req ->
        match route with
        | Home -> Dream.respond "Home page!"
        | About _ -> Dream.respond "About page!"
        | Echo { test; nb } ->
            (* let nb =
                 nb |> List.map ~f:string_of_int |> String.concat ~sep:", "
               in *)
            Dream.respond
              (Printf.sprintf "Echo with test = %b; nb = %d\n" test nb)
        | Hello { name; repeat } ->
            let name =
              match repeat with
              | Some repeat ->
                  List.init ~len:repeat ~f:(fun _ -> name)
                  |> String.concat ~sep:", "
              | None -> name
            in
            Dream.respond (Printf.sprintf "Hello, %s" name))
end

let req = Dream.request ~method_:`GET ~target:"/About?active=false" ""

let () =
  let main () =
    let open Lwt.Syntax in
    let* res = Handler.pages_handle req in
    let* body = Dream.body res in
    Printf.printf "%s\n" body;
    Lwt.return_unit
  in

  Lwt_main.run (main ())
