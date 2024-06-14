[@@@warning "-34-37-32"]

type 'v route = Route : ('a, 'v) Routes.path * 'a * ('v -> 'w) -> 'w route

let home () = Routes.(s "home" / str /? nil)
let project () = Routes.(s "project" / int /? nil)

(* type response = int * (string * string) list * string *)

(* type _ encode =
   | Encode_raw : response encode
   | Encode_json : ('a -> string) -> 'a encode *)

type my_routes =
  | Route_home of { name : string }
  | Route_project of { nb : int }
(* type packed = Packed : my_routes * response encode -> packed *)

let r1 =
  Route
    (home (), (fun s (_req : Dream.request) -> Route_home { name = s }), Fun.id)

let r2 =
  Route
    ( project (),
      (fun i (_req : Dream.request) -> Route_project { nb = i }),
      Fun.id )

let to_route (Route (path, a, f)) = Routes.(map f (route path a))
let router = [ r1; r2 ] |> List.map to_route |> Routes.one_of

type handler = my_routes -> Dream.request -> string

let handle route (_req : Dream.request) : string =
  match route with
  | Route_home { name } -> Printf.sprintf "Route_home with name = %s\n" name
  | Route_project { nb } -> Printf.sprintf "Route_project with nb = %d\n" nb

let response =
  let request = Dream.request ~method_:`GET ~target:"/project/2" "" in
  let route =
    match Routes.match' router ~target:(Dream.target request) with
    | Routes.FullMatch f | Routes.MatchWithTrailingSlash f -> f request
    | Routes.NoMatch -> assert false
  in
  handle route request

let () = print_endline response
