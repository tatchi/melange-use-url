module Pages =
  struct
    open Ppx_deriving_router_runtime.Primitives
    type t =
      | Home [@GET "/"]
      | About 
      | Hello of {
      name: string ;
      repeat: int option } [@GET "/hello/:name"][@@deriving router]
    include
      struct
        let _ = fun (_ : t) -> ()
        let href =
          (function
           | Home -> "/"
           | About ->
               let out__016_ = Buffer.create 16 in
               let _sep__017_ = ref '?' in
               (Buffer.add_char out__016_ '/';
                Ppx_deriving_router_runtime.Encode.encode_path out__016_
                  "About";
                Buffer.contents out__016_)
           | Hello _x__018_ ->
               let name = _x__018_.name
               and repeat = _x__018_.repeat in
               let out__019_ = Buffer.create 16 in
               let _sep__020_ = ref '?' in
               (Buffer.add_char out__019_ '/';
                Ppx_deriving_router_runtime.Encode.encode_path out__019_
                  "hello";
                Buffer.add_char out__019_ '/';
                Ppx_deriving_router_runtime.Encode.encode_path out__019_
                  (string_to_url_path name);
                Stdlib.List.iter
                  (fun (name, value) ->
                     Buffer.add_char out__019_ (!_sep__020_);
                     Ppx_deriving_router_runtime.Encode.encode_query_key
                       out__019_ name;
                     _sep__020_ := '&';
                     Buffer.add_char out__019_ '=';
                     Ppx_deriving_router_runtime.Encode.encode_query_value
                       out__019_ value)
                  ((option_to_url_query int_to_url_query) "repeat" repeat);
                Buffer.contents out__019_) : t -> string)
        let _ = href
        let http_method =
          (function | Home -> `GET | About -> `GET | Hello _x__015_ -> `GET : 
          t -> [ `GET  | `POST  | `PUT  | `DELETE ])
        let _ = http_method
        let body =
          (function | Home -> None | About -> None | Hello _x__012_ -> None : 
          t -> Ppx_deriving_router_runtime.json option)
        let _ = body
        let encode_response =
          (fun route ->
             fun _value ->
               match route with
               | Home -> failwith "response cannot be serialized to json"
               | About -> failwith "response cannot be serialized to json"
               | Hello _x__009_ ->
                   failwith "response cannot be serialized to json" : 
          t ->
            Ppx_deriving_router_runtime.response ->
              Ppx_deriving_router_runtime.json)
        let _ = encode_response
        type packed =
          | Packed: t * Ppx_deriving_router_runtime.response
          Ppx_deriving_router_runtime.Handle.encode -> packed 
        type handler =
          {
          f:
            t ->
              Ppx_deriving_router_runtime.request ->
                Ppx_deriving_router_runtime.response
                  Ppx_deriving_router_runtime.return Lwt.t
            }
        let routes =
          let path_Home =
            [Ppx_deriving_router_runtime.Handle.Route
               (Routes.nil,
                 ((fun (_req__006_ : Ppx_deriving_router_runtime.request) ->
                     match Ppx_deriving_router_runtime.Request.method_
                             _req__006_
                     with
                     | `GET ->
                         let __url_query =
                           Ppx_deriving_router_runtime.Request.queries
                             _req__006_ in
                         Lwt.return
                           (Packed
                              (Home,
                                Ppx_deriving_router_runtime.Handle.Encode_raw))
                     | _ ->
                         raise
                           Ppx_deriving_router_runtime.Handle.Method_not_allowed)),
                 Stdlib.Fun.id)]
          and path_About =
            [Ppx_deriving_router_runtime.Handle.Route
               ((Routes.(/?) (Routes.s "About") Routes.nil),
                 ((fun (_req__005_ : Ppx_deriving_router_runtime.request) ->
                     match Ppx_deriving_router_runtime.Request.method_
                             _req__005_
                     with
                     | `GET ->
                         let __url_query =
                           Ppx_deriving_router_runtime.Request.queries
                             _req__005_ in
                         Lwt.return
                           (Packed
                              (About,
                                Ppx_deriving_router_runtime.Handle.Encode_raw))
                     | _ ->
                         raise
                           Ppx_deriving_router_runtime.Handle.Method_not_allowed)),
                 Stdlib.Fun.id)]
          and path_Hello =
            [Ppx_deriving_router_runtime.Handle.Route
               ((Routes.(/?)
                   (Routes.(/) (Routes.s "hello")
                      (Routes.pattern string_to_url_path string_of_url_path
                         "name")) Routes.nil),
                 ((fun _param0 ->
                     fun (_req__004_ : Ppx_deriving_router_runtime.request)
                       ->
                       match Ppx_deriving_router_runtime.Request.method_
                               _req__004_
                       with
                       | `GET ->
                           let __url_query =
                             Ppx_deriving_router_runtime.Request.queries
                               _req__004_ in
                           Lwt.return
                             (Packed
                                ((Hello
                                    {
                                      name = _param0;
                                      repeat =
                                        (match (option_of_url_query
                                                  int_of_url_query) "repeat"
                                                 __url_query
                                         with
                                         | Ok v -> v
                                         | Error err ->
                                             raise
                                               (Ppx_deriving_router_runtime.Handle.Invalid_query_parameter
                                                  ("repeat", err)))
                                    }),
                                  Ppx_deriving_router_runtime.Handle.Encode_raw))
                       | _ ->
                           raise
                             Ppx_deriving_router_runtime.Handle.Method_not_allowed)),
                 Stdlib.Fun.id)] in
          Stdlib.List.flatten [path_Hello; path_About; path_Home]
        let _ = routes
        let handle =
          let router =
            Ppx_deriving_router_runtime.Handle.make
              (let routes =
                 Stdlib.List.map Ppx_deriving_router_runtime.Handle.to_route
                   routes in
               Routes.one_of routes) in
          fun ({ f } : handler) ->
            Ppx_deriving_router_runtime.Handle.handle router
              (fun (Packed (p, encode)) ->
                 fun req ->
                   Lwt.bind (f p req)
                     (Ppx_deriving_router_runtime.Handle.encode encode))
        let _ = handle
        let handle f = handle { f }
        let _ = handle
        open
          struct
            let (witness_Home :
              Ppx_deriving_router_runtime.response
                Ppx_deriving_router_runtime.Witness.t)
              = Ppx_deriving_router_runtime.Witness.create ()
            let _ = witness_Home
            let (witness_About :
              Ppx_deriving_router_runtime.response
                Ppx_deriving_router_runtime.Witness.t)
              = Ppx_deriving_router_runtime.Witness.create ()
            let _ = witness_About
            let (witness_Hello :
              Ppx_deriving_router_runtime.response
                Ppx_deriving_router_runtime.Witness.t)
              = Ppx_deriving_router_runtime.Witness.create ()
            let _ = witness_Hello
          end
        let witness =
          (function
           | Home -> witness_Home
           | About -> witness_About
           | Hello _x__003_ -> witness_Hello : t ->
                                                 Ppx_deriving_router_runtime.response
                                                   Ppx_deriving_router_runtime.Witness.t)
        let _ = witness
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
