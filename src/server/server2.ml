[@@@ocaml.ppx.context
{
  tool_name = "ppx_driver";
  include_dirs = [];
  load_path = [];
  open_modules = [];
  for_package = None;
  debug = false;
  use_threads = false;
  use_vmthreads = false;
  recursive_types = false;
  principal = false;
  transparent_modules = false;
  unboxed_types = false;
  unsafe_string = false;
  cookies = [];
}]

module Pages = struct
  open Ppx_deriving_router_runtime.Primitives

  type t =
    | Home [@GET "/"]
    | About
    | Echo of { test : bool; nb : int }
    | Hello of { name : string; repeat : int option } [@GET "/hello/:name"]
  [@@deriving router]

  include struct
    let _ = fun (_ : t) -> ()

    let href =
      (function
       | Home -> "/"
       | About ->
           let out__021_ = Buffer.create 16 in
           Buffer.add_char out__021_ '/';
           Ppx_deriving_router_runtime.Encode.encode_path out__021_ "About";
           Buffer.contents out__021_
       | Echo _x__023_ ->
           let test = _x__023_.test and nb = _x__023_.nb in
           let out__024_ = Buffer.create 16 in
           Buffer.add_char out__024_ '/';
           Ppx_deriving_router_runtime.Encode.encode_path out__024_ "Echo";
           let _sep__025_ = ref '?' in
           Stdlib.List.iter
             (fun (name, value) ->
               Buffer.add_char out__024_ !_sep__025_;
               Ppx_deriving_router_runtime.Encode.encode_query_key out__024_
                 name;
               _sep__025_ := '&';
               Buffer.add_char out__024_ '=';
               Ppx_deriving_router_runtime.Encode.encode_query_value out__024_
                 value)
             (int_to_url_query "nb" nb);
           Stdlib.List.iter
             (fun (name, value) ->
               Buffer.add_char out__024_ !_sep__025_;
               Ppx_deriving_router_runtime.Encode.encode_query_key out__024_
                 name;
               _sep__025_ := '&';
               Buffer.add_char out__024_ '=';
               Ppx_deriving_router_runtime.Encode.encode_query_value out__024_
                 value)
             (bool_to_url_query "test" test);
           Buffer.contents out__024_
       | Hello _x__026_ ->
           let name = _x__026_.name and repeat = _x__026_.repeat in
           let out__027_ = Buffer.create 16 in
           Buffer.add_char out__027_ '/';
           Ppx_deriving_router_runtime.Encode.encode_path out__027_ "hello";
           Buffer.add_char out__027_ '/';
           Ppx_deriving_router_runtime.Encode.encode_path out__027_
             (string_to_url_path name);
           let _sep__028_ = ref '?' in
           Stdlib.List.iter
             (fun (name, value) ->
               Buffer.add_char out__027_ !_sep__028_;
               Ppx_deriving_router_runtime.Encode.encode_query_key out__027_
                 name;
               _sep__028_ := '&';
               Buffer.add_char out__027_ '=';
               Ppx_deriving_router_runtime.Encode.encode_query_value out__027_
                 value)
             ((option_to_url_query int_to_url_query) "repeat" repeat);
           Buffer.contents out__027_
        : t -> string)

    let _ = href

    let http_method =
      (function
       | Home -> `GET
       | About -> `GET
       | Echo _x__019_ -> `GET
       | Hello _x__020_ -> `GET
        : t -> [ `GET | `POST | `PUT | `DELETE ])

    let _ = http_method

    let body =
      (function
       | Home -> None
       | About -> None
       | Echo _x__015_ -> None
       | Hello _x__016_ -> None
        : t -> Ppx_deriving_router_runtime.json option)

    let _ = body

    let encode_response =
      (fun route _value ->
         match route with
         | Home -> failwith "response cannot be serialized to json"
         | About -> failwith "response cannot be serialized to json"
         | Echo _x__011_ -> failwith "response cannot be serialized to json"
         | Hello _x__012_ -> failwith "response cannot be serialized to json"
        : t ->
          Ppx_deriving_router_runtime.response ->
          Ppx_deriving_router_runtime.json)

    let _ = encode_response

    type packed =
      | Packed :
          t
          * Ppx_deriving_router_runtime.response
            Ppx_deriving_router_runtime.Handle.encode
          -> packed

    type handler = {
      f :
        t ->
        Ppx_deriving_router_runtime.request ->
        Ppx_deriving_router_runtime.response Ppx_deriving_router_runtime.return
        Lwt.t;
    }

    let routes =
      let path_Home =
        [
          Ppx_deriving_router_runtime.Handle.Route
            ( Routes.nil,
              (fun (_req__008_ : Ppx_deriving_router_runtime.request) ->
                match
                  Ppx_deriving_router_runtime.Request.method_ _req__008_
                with
                | `GET ->
                    let __url_query =
                      Ppx_deriving_router_runtime.Request.queries _req__008_
                    in
                    Lwt.return
                      (Packed
                         (Home, Ppx_deriving_router_runtime.Handle.Encode_raw))
                | _ ->
                    raise Ppx_deriving_router_runtime.Handle.Method_not_allowed),
              Stdlib.Fun.id );
        ]
      and path_About =
        [
          Ppx_deriving_router_runtime.Handle.Route
            ( Routes.( /? ) (Routes.s "About") Routes.nil,
              (fun (_req__007_ : Ppx_deriving_router_runtime.request) ->
                match
                  Ppx_deriving_router_runtime.Request.method_ _req__007_
                with
                | `GET ->
                    let __url_query =
                      Ppx_deriving_router_runtime.Request.queries _req__007_
                    in
                    Lwt.return
                      (Packed
                         (About, Ppx_deriving_router_runtime.Handle.Encode_raw))
                | _ ->
                    raise Ppx_deriving_router_runtime.Handle.Method_not_allowed),
              Stdlib.Fun.id );
        ]
      and path_Hello =
        [
          Ppx_deriving_router_runtime.Handle.Route
            ( Routes.( /? )
                (Routes.( / ) (Routes.s "hello")
                   (Routes.pattern string_to_url_path string_of_url_path "name"))
                Routes.nil,
              (fun _param0 (_req__006_ : Ppx_deriving_router_runtime.request) ->
                match
                  Ppx_deriving_router_runtime.Request.method_ _req__006_
                with
                | `GET ->
                    let __url_query =
                      Ppx_deriving_router_runtime.Request.queries _req__006_
                    in
                    Lwt.return
                      (Packed
                         ( Hello
                             {
                               name = _param0;
                               repeat =
                                 (match
                                    (option_of_url_query int_of_url_query)
                                      "repeat" __url_query
                                  with
                                 | Ok v -> v
                                 | Error err ->
                                     raise
                                       (Ppx_deriving_router_runtime.Handle
                                        .Invalid_query_parameter
                                          ("repeat", err)));
                             },
                           Ppx_deriving_router_runtime.Handle.Encode_raw ))
                | _ ->
                    raise Ppx_deriving_router_runtime.Handle.Method_not_allowed),
              Stdlib.Fun.id );
        ]
      and path_Echo =
        [
          Ppx_deriving_router_runtime.Handle.Route
            ( Routes.( /? ) (Routes.s "Echo") Routes.nil,
              (fun (_req__005_ : Ppx_deriving_router_runtime.request) ->
                match
                  Ppx_deriving_router_runtime.Request.method_ _req__005_
                with
                | `GET ->
                    let __url_query =
                      Ppx_deriving_router_runtime.Request.queries _req__005_
                    in
                    Lwt.return
                      (Packed
                         ( Echo
                             {
                               test =
                                 (match
                                    bool_of_url_query "test" __url_query
                                  with
                                 | Ok v -> v
                                 | Error err ->
                                     raise
                                       (Ppx_deriving_router_runtime.Handle
                                        .Invalid_query_parameter
                                          ("test", err)));
                               nb =
                                 (match int_of_url_query "nb" __url_query with
                                 | Ok v -> v
                                 | Error err ->
                                     raise
                                       (Ppx_deriving_router_runtime.Handle
                                        .Invalid_query_parameter
                                          ("nb", err)));
                             },
                           Ppx_deriving_router_runtime.Handle.Encode_raw ))
                | _ ->
                    raise Ppx_deriving_router_runtime.Handle.Method_not_allowed),
              Stdlib.Fun.id );
        ]
      in
      Stdlib.List.flatten [ path_Echo; path_Hello; path_About; path_Home ]

    let _ = routes

    let handle =
      let router =
        Ppx_deriving_router_runtime.Handle.make
          (let routes =
             Stdlib.List.map Ppx_deriving_router_runtime.Handle.to_route routes
           in
           Routes.one_of routes)
      in
      fun ({ f } : handler) ->
        Ppx_deriving_router_runtime.Handle.handle router
          (fun (Packed (p, encode)) req ->
            Lwt.bind (f p req)
              (Ppx_deriving_router_runtime.Handle.encode encode))

    let _ = handle
    let handle f = handle { f }
    let _ = handle

    open struct
      let (witness_Home :
            Ppx_deriving_router_runtime.response
            Ppx_deriving_router_runtime.Witness.t) =
        Ppx_deriving_router_runtime.Witness.create ()

      let _ = witness_Home

      let (witness_About :
            Ppx_deriving_router_runtime.response
            Ppx_deriving_router_runtime.Witness.t) =
        Ppx_deriving_router_runtime.Witness.create ()

      let _ = witness_About

      let (witness_Echo :
            Ppx_deriving_router_runtime.response
            Ppx_deriving_router_runtime.Witness.t) =
        Ppx_deriving_router_runtime.Witness.create ()

      let _ = witness_Echo

      let (witness_Hello :
            Ppx_deriving_router_runtime.response
            Ppx_deriving_router_runtime.Witness.t) =
        Ppx_deriving_router_runtime.Witness.create ()

      let _ = witness_Hello
    end

    let witness =
      (function
       | Home -> witness_Home
       | About -> witness_About
       | Echo _x__003_ -> witness_Echo
       | Hello _x__004_ -> witness_Hello
        : t ->
          Ppx_deriving_router_runtime.response
          Ppx_deriving_router_runtime.Witness.t)

    let _ = witness
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
end

let _hello = Pages.href (Hello { name = "corentin"; repeat = Some 2 })
let echo = Pages.href (Echo { test = true; nb = 4 })
let () = print_endline echo
