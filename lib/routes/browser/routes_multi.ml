[@@@warning "-60-34-37"]

module Util = struct
  let split_path target =
    let split_target target =
      match target with
      | "" | "/" -> []
      | _ -> (
          match String.split_on_char '/' target with "" :: xs -> xs | xs -> xs)
    in
    match String.index_opt target '?' with
    | None -> split_target target
    | Some 0 -> []
    | Some i -> split_target (String.sub target 0 i)
end

module PatternTrie = struct
  module Key = struct
    type t = Match : string -> t | Capture : string -> t [@@deriving show]
  end

  module KeyMap = Map.Make (String)

  (* let pp t = print_endline @@ [%show: (string * char) list] (KeyMap.bindings
     t) *)

  type 'a node = {
    parsers : 'a list;
    children : 'a node KeyMap.t;
    capture : (string * 'a node) option;
  }

  type 'a t = 'a node

  let empty = { parsers = []; children = KeyMap.empty; capture = None }

  let feed_params t params =
    let rec aux t params =
      match (t, params) with
      | { parsers = []; _ }, [] -> []
      | { parsers = rs; _ }, [] -> rs
      | { parsers = rs; _ }, [ "" ] -> rs
      | { children; capture; _ }, x :: xs -> (
          Printf.printf "find_opt x = %s\n" x;
          match KeyMap.find_opt x children with
          | None -> (
              Printf.printf "[%s] not found, let's see capture\n" x;
              match capture with
              | None ->
                  Printf.printf "[%s] capture not found\n" x;
                  []
              | Some (_, t') ->
                  Printf.printf "[%s] capture  found\n" x;
                  aux t' xs)
          | Some m' ->
              Printf.printf "[%s] FOUND!\n" x;
              aux m' xs)
    in
    aux t params

  let feed_params_2 t params =
    let rec aux t params acc =
      match (t, params) with
      (* | { parsers = []; _ }, [] -> [] *)
      (* | { parsers = _ :: _; _ }, [] -> acc *)
      | _, [] -> acc
      | _, [ "" ] -> acc
      | { children; capture; _ }, x :: xs -> (
          Printf.printf "find_opt x = %s\n" x;
          match KeyMap.find_opt x children with
          | None -> (
              Printf.printf "[%s] not found, let's see capture\n" x;
              match capture with
              | None ->
                  Printf.printf "[%s] capture not found\n" x;
                  []
              | Some (_, t') ->
                  Printf.printf "[%s] capture  found\n" x;
                  aux t' xs (t'.parsers @ acc))
          | Some m' ->
              let p = m'.parsers in
              Printf.printf "[%s] FOUND!\n" x;
              aux m' xs (p @ acc))
    in
    aux t params []

  let add k v t =
    (* let show_patterns p = [%show: Key.t list] p in *)
    (* Printf.printf "Inserting patterns = %s\n\n" (show_patterns k); *)
    let rec aux k t =
      match (k, t) with
      | [], ({ parsers = x; _ } as n) -> { n with parsers = v :: x }
      | x :: r, ({ children; capture; _ } as n) -> (
          match x with
          | Key.Match w ->
              let t' =
                match KeyMap.find_opt w children with
                | None -> empty
                | Some v -> v
              in
              let t'' = aux r t' in
              { n with children = KeyMap.add w t'' children }
          | Key.Capture c ->
              let t' = match capture with None -> empty | Some (_, v) -> v in
              let t'' = aux r t' in
              { n with capture = Some (c, t'') })
    in
    aux k t
end

type 'a conv = {
  to_ : 'a -> string;
  from_ : string -> 'a option;
  label : string;
}
[@@warning "-69"]

let conv to_ from_ label = { to_; from_; label }

type ('a, 'b) path =
  | End : ('a, 'a) path
  (* | Wildcard : (Parts.t -> 'a, 'a) path *)
  | Match : string * ('a, 'b) path -> ('a, 'b) path
  | Conv : 'c conv * ('a, 'b) path -> ('c -> 'a, 'b) path

(* let rec show_path : type a b. (a, b) path -> string = function
   | End -> "End"
   | Match (w, r) -> Printf.sprintf "Match(%s,%s)" w (show_path r)
   | Conv ({ label; _ }, r) -> Printf.sprintf "Conv(%s, %s)" label (show_path r) *)

type 'b route = Route : ('a, 'b) path * 'a -> 'b route
type 'b router = 'b route PatternTrie.t

(* let show_route (route : 'b route) =
     let (Route (path, _)) = route in
     show_path path

   let show_routes (routes : 'b route list) =
     let s = routes |> List.map show_route |> String.concat "; " in
     Printf.sprintf "[%s]" s *)

(* let yojson_of_routes (routes : 'b route list) : Yojson.Safe.t =
   let s = routes |> List.map (fun r -> `String (show_route r)) in
   `List s *)

let empty_router = PatternTrie.empty
let of_conv conv r = Conv (conv, r)

let int name r =
  of_conv
    (conv string_of_int int_of_string_opt (Printf.sprintf "%s:int" name))
    r

let str name r =
  of_conv (conv Fun.id (fun x -> Some x) (Printf.sprintf "%s:string" name)) r

let s w r = Match (w, r)
let nil = End
let route r handler = Route (r, handler)
let ( / ) m1 m2 r = m1 @@ m2 r
let ( /? ) m1 m2 = m1 m2
let ( @--> ) = route

let rec route_pattern : type a b. (a, b) path -> PatternTrie.Key.t list =
  function
  | End -> []
  | Match (w, r) -> PatternTrie.Key.Match w :: route_pattern r
  | Conv ({ label; _ }, r) -> PatternTrie.Key.Capture label :: route_pattern r

let one_of routes =
  let routes = List.rev routes in
  List.fold_left
    (fun routes (Route (path, _) as route) ->
      let patterns = route_pattern path in
      PatternTrie.add patterns route routes)
    empty_router routes

let add_route route router =
  let (Route (path, _)) = route in
  let patterns = route_pattern path in
  PatternTrie.add patterns route router

type 'a match_result = FullMatch of 'a list | NoMatch

let parse_route path handler target =
  let rec match_target : type a b. (a, b) path -> a -> string list -> b list =
   fun path handler target ->
    match path with
    | End -> (
        match target with [] | [ "" ] -> [ handler ] | _ -> [ handler ])
    | Match (w, r) -> (
        match target with
        | x :: xs when x = w -> match_target r handler xs
        | _ -> [])
    | Conv ({ from_; _ }, r) -> (
        match target with
        | [] -> []
        | x :: xs -> (
            match from_ x with
            | None -> []
            | Some v -> match_target r (handler v) xs))
  in
  match_target path handler target

let rec match_routes target routes acc =
  match routes with
  | [] -> acc
  | Route (r, h) :: rs -> (
      match parse_route r h target with
      | [] ->
          Printf.printf "[match_routes]: No match\n";
          match_routes target rs acc
      | r ->
          Printf.printf "[match_routes]: Got a match\n";
          match_routes target rs (r @ acc))

let match' router ~target =
  let target = Util.split_path target in
  let routes = PatternTrie.feed_params_2 router target in
  (* Printf.printf "matching routes = %s\n" (show_routes routes); *)
  let res = match_routes target routes [] in
  match res with [] -> NoMatch | l -> FullMatch l

let ksprintf' k path =
  let rec aux : type a b. (string list -> b) -> (a, b) path -> a =
   fun k -> function
    | End -> k []
    (* | Wildcard -> fun { Parts.matched; _ } -> k (List.concat [ matched; [] ]) *)
    | Match (w, fmt) -> aux (fun s -> k @@ (w :: s)) fmt
    | Conv ({ to_; _ }, fmt) ->
        fun x -> aux (fun rest -> k @@ (to_ x :: rest)) fmt
  in
  aux k path

let ksprintf k t = ksprintf' (fun x -> k ("/" ^ String.concat "/" x)) t
let sprintf t = ksprintf (fun x -> x) t
