[@@@warning "-32-33"]

open StdLabels

module Project_details = struct
  open Ppx_deriving_router_runtime.Primitives

  type t = Root of { id : int } [@GET "/:id"] | Tasks [@GET "/tasks"]
  [@@deriving router]
end

module Projects = struct
  open Ppx_deriving_router_runtime.Primitives

  type _ t =
    | Root : Ppx_deriving_router_runtime.response t [@GET "/"]
    | Id : Project_details.t -> Ppx_deriving_router_runtime.response t
        [@prefix ""]
  [@@deriving router]
end

module Root = struct
  open Ppx_deriving_router_runtime.Primitives

  type _ t =
    | Home : Ppx_deriving_router_runtime.response t [@GET "/home"]
    | Projects : 'a Projects.t -> 'a t [@prefix "/projects"]
  [@@deriving router]
end

let () =
  Printf.printf "Home root = %s\n" (Root.href Home);
  Printf.printf "Projects = %s\n" (Root.href (Projects Root));
  Printf.printf "Project_details root = %s\n"
    (Root.href (Projects (Id (Project_details.Root { id = 5 }))));

  Printf.printf "Project_details tasks = %s\n"
    (Root.href (Projects (Id Project_details.Tasks)))
