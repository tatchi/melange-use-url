open StdLabels

module Pages = struct
  open Ppx_deriving_router_runtime.Primitives

  type t =
    | Home [@GET "/home"]
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

module Pages_handle = struct
  let handle =
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

module Api = struct
  open Ppx_deriving_router_runtime.Primitives
  open Ppx_deriving_json_runtime.Primitives

  type user = { id : int } [@@deriving json]

  type _ t =
    | List_users : user list t [@GET "/"]
    | Create_user : user t [@POST "/"]
    | Get_user : { id : int; active : bool } -> user t [@GET "/:id"]
    | Raw : Ppx_deriving_router_runtime.response t [@GET "/raw"]
  [@@deriving router]
end

module Api_handle = struct
  let handle : Dream.handler =
    let f : type a. a Api.t -> Dream.request -> a Lwt.t =
     fun x _req ->
      match x with
      | List_users -> Lwt.return [ { Api.id = 1 } ]
      | Create_user -> Lwt.return { Api.id = 42 }
      | Get_user { id; _ } -> Lwt.return { Api.id }
      | Raw -> Dream.respond "RAW"
    in
    Api.handle { f }
end

let req = Dream.request ~method_:`GET ~target:"/About?active=false" ""
let api_req = Dream.request ~method_:`GET ~target:(Api.href Api.List_users) ""

let () =
  let main () =
    let open Lwt.Syntax in
    let* res = Pages_handle.handle req in
    let* body = Dream.body res in
    Printf.printf "%s\n" body;
    let* res = Api_handle.handle api_req in
    let* body = Dream.body res in
    Printf.printf "%s\n" body;
    Printf.printf "%s\n" (Api.href (Api.Get_user { id = 4; active = true }));
    Lwt.return_unit
  in

  Lwt_main.run (main ())
