module Util : sig
  val split_path : string -> string list
end

module PatternTrie : sig
  module Key : sig
    type t = Match : string -> t | Capture : string -> t [@@deriving show]
  end

  module KeyMap : Map.S with type key = string

  (* let pp t = print_endline @@ [%show: (string * char) list] (KeyMap.bindings
     t) *)

  type 'a node = {
    parsers : 'a list;
    children : 'a node KeyMap.t;
    capture : (string * 'a node) option;
  }

  type 'a t = 'a node

  val empty : 'a t
  val add : Key.t list -> 'a -> 'a t -> 'a t
  val feed_params : 'a t -> string list -> 'a list
  val feed_params_2 : 'a t -> string list -> 'a list
end

(** Typed routing for OCaml. [Routes] provides combinators for adding typed
    routing to OCaml applications. The core library will be independent of any
    particular web framework or runtime. *)

type ('a, 'b) path
(** [path] represents a sequence of path parameter patterns that are expected in
    a route. *)

(** [route] is a combination of a path sequence, with a function that will be
    called on a successful match. When a path sequence matches, the patterns
    that are extracted are forwarded to said function with the types that the
    user defined. Note that because of
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/polymorphism.html#ss:valuerestriction}
      value restriction}, the route definitions will be assigned a weak type by
    the compiler. This causes problems if one intends to re-use the same route
    definition in multiple contexts, like using a single definition for both
    matching a target url, and serializing to use in a client call. To avoid
    such problems one can use eta-expansion (i.e. add an explicit argument to
    the route definition).

    Example:

    {[
      let route () =
        Routes.(
          (s "foo" / str / int /? nil) @--> fun (a : string) (b : int) ->
          Printf.sprintf "%s %d" a b)
    ]} *)

type 'b route = Route : ('a, 'b) path * 'a -> 'b route

type 'b router = 'b route PatternTrie.t
(** [router] is a collection of multiple routes. It transforms a list of routes
    into a trie like structure, that is then used for matching an input target
    url.*)

val empty_router : 'b router
(* val show_routes : 'b route list -> string *)

(* val yojson_of_routes : 'b route list -> Yojson.Safe.t *)
(* val show_route : 'b route -> string *)
val int : string -> ('a, 'b) path -> (int -> 'a, 'b) path
val str : string -> ('a, 'b) path -> (string -> 'a, 'b) path
val s : string -> ('a, 'b) path -> ('a, 'b) path
val nil : ('a, 'a) path

val ( / ) : (('a, 'b) path -> 'c) -> ('d -> ('a, 'b) path) -> 'd -> 'c
(** [l / r] joins two path match patterns [l] and [r] into a pattern sequence,
    parse l followed by parse r. Example: If we want to define a route that
    matches a string followd by a constant "foo" and then an integer, we'd use
    the [/] operator like below:

    {[
      let route () = Routes.(str / s "foo" / int /? nil)
    ]} *)

val ( /? ) : (('a, 'b) path -> ('c, 'd) path) -> ('a, 'b) path -> ('c, 'd) path

val ( @--> ) : ('a, 'b) path -> 'a -> 'b route
(** [r @--> h] is used to connect a route pattern [r] to a function [h] that
    gets called if this pattern is successfully matched.*)

val route : ('a, 'b) path -> 'a -> 'b route
(** [route r h] is the same as [r @--> h]. It is used to connect a route pattern
    [r] to a function [h] that gets called if the pattern is a successfully
    matched.

    @since 2.0.0 *)

val one_of : 'b route list -> 'b router
(** [one_of] accepts a list of tuples comprised of route definitions of type
    ['b route] where 'b is the type that a successful route match will return.

    It transforms the input list of routes into a trie like structure that can
    later be used to perform route matches. *)

val route_pattern : ('a, 'b) path -> PatternTrie.Key.t list
val add_route : 'b route -> 'b router -> 'b router

type 'a match_result = FullMatch of 'a list | NoMatch

val match' : 'a router -> target:string -> 'a match_result
(** [match'] accepts a router and the target url to match. *)

val sprintf : ('a, string) path -> 'a
(** [sprintf] takes a route pattern as an input, and returns a string with the result of
    formatting the pattern into a URI path. *)
