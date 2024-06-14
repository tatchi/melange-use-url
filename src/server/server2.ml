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

open StdLabels

module Pages = struct
  open Ppx_deriving_router_runtime.Primitives

  type t =
    | Home [@GET "/home"]
    | About of { active : bool }
    | Echo of { test : bool; nb : int }
    | Hello of { name : string; repeat : int option } [@GET "/hello/:name"]
  [@@deriving router]

  include struct
    let _ = fun (_ : t) -> ()

    let href =
      (function
       | Home ->
           let out__021_ = Buffer.create 16 in
           Buffer.add_char out__021_ '/';
           Ppx_deriving_router_runtime.Encode.encode_path out__021_ "home";
           Buffer.contents out__021_
       | About _x__023_ ->
           let active = _x__023_.active in
           let out__024_ = Buffer.create 16 in
           Buffer.add_char out__024_ '/';
           Ppx_deriving_router_runtime.Encode.encode_path out__024_ "About";
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
             (bool_to_url_query "active" active);
           Buffer.contents out__024_
       | Echo _x__026_ ->
           let test = _x__026_.test and nb = _x__026_.nb in
           let out__027_ = Buffer.create 16 in
           Buffer.add_char out__027_ '/';
           Ppx_deriving_router_runtime.Encode.encode_path out__027_ "Echo";
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
             (int_to_url_query "nb" nb);
           Stdlib.List.iter
             (fun (name, value) ->
               Buffer.add_char out__027_ !_sep__028_;
               Ppx_deriving_router_runtime.Encode.encode_query_key out__027_
                 name;
               _sep__028_ := '&';
               Buffer.add_char out__027_ '=';
               Ppx_deriving_router_runtime.Encode.encode_query_value out__027_
                 value)
             (bool_to_url_query "test" test);
           Buffer.contents out__027_
       | Hello _x__029_ ->
           let name = _x__029_.name and repeat = _x__029_.repeat in
           let out__030_ = Buffer.create 16 in
           Buffer.add_char out__030_ '/';
           Ppx_deriving_router_runtime.Encode.encode_path out__030_ "hello";
           Buffer.add_char out__030_ '/';
           Ppx_deriving_router_runtime.Encode.encode_path out__030_
             (string_to_url_path name);
           let _sep__031_ = ref '?' in
           Stdlib.List.iter
             (fun (name, value) ->
               Buffer.add_char out__030_ !_sep__031_;
               Ppx_deriving_router_runtime.Encode.encode_query_key out__030_
                 name;
               _sep__031_ := '&';
               Buffer.add_char out__030_ '=';
               Ppx_deriving_router_runtime.Encode.encode_query_value out__030_
                 value)
             ((option_to_url_query int_to_url_query) "repeat" repeat);
           Buffer.contents out__030_
        : t -> string)

    let _ = href

    let http_method =
      (function
       | Home -> `GET
       | About _x__018_ -> `GET
       | Echo _x__019_ -> `GET
       | Hello _x__020_ -> `GET
        : t -> [ `GET | `POST | `PUT | `DELETE ])

    let _ = http_method

    let body =
      (function
       | Home -> None
       | About _x__014_ -> None
       | Echo _x__015_ -> None
       | Hello _x__016_ -> None
        : t -> Ppx_deriving_router_runtime.json option)

    let _ = body

    let encode_response =
      (fun route _value ->
         match route with
         | Home -> failwith "response cannot be serialized to json"
         | About _x__010_ -> failwith "response cannot be serialized to json"
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
      let path_About =
        [
          Ppx_deriving_router_runtime.Handle.Route
            ( Routes.( /? ) (Routes.s "About") Routes.nil,
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
                         ( About
                             {
                               active =
                                 (match
                                    bool_of_url_query "active" __url_query
                                  with
                                 | Ok v -> v
                                 | Error err ->
                                     raise
                                       (Ppx_deriving_router_runtime.Handle
                                        .Invalid_query_parameter
                                          ("active", err)));
                             },
                           Ppx_deriving_router_runtime.Handle.Encode_raw ))
                | _ ->
                    raise Ppx_deriving_router_runtime.Handle.Method_not_allowed),
              Stdlib.Fun.id );
        ]
      and path_Hello =
        [
          Ppx_deriving_router_runtime.Handle.Route
            ( Routes.( /? )
                (Routes.( / ) (Routes.s "hello")
                   (Routes.pattern
                      (fun x ->
                        let x = string_to_url_path x in
                        let buf = Buffer.create (String.length x) in
                        Ppx_deriving_router_runtime.Encode.encode_path buf x;
                        Buffer.contents buf)
                      (fun x ->
                        let x =
                          Ppx_deriving_router_runtime.Decode.decode_path x
                        in
                        string_of_url_path x)
                      "name"))
                Routes.nil,
              (fun _param0 (_req__007_ : Ppx_deriving_router_runtime.request) ->
                match
                  Ppx_deriving_router_runtime.Request.method_ _req__007_
                with
                | `GET ->
                    let __url_query =
                      Ppx_deriving_router_runtime.Request.queries _req__007_
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
              (fun (_req__006_ : Ppx_deriving_router_runtime.request) ->
                match
                  Ppx_deriving_router_runtime.Request.method_ _req__006_
                with
                | `GET ->
                    let __url_query =
                      Ppx_deriving_router_runtime.Request.queries _req__006_
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
      and path_Home =
        [
          Ppx_deriving_router_runtime.Handle.Route
            ( Routes.( /? ) (Routes.s "home") Routes.nil,
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
                         (Home, Ppx_deriving_router_runtime.Handle.Encode_raw))
                | _ ->
                    raise Ppx_deriving_router_runtime.Handle.Method_not_allowed),
              Stdlib.Fun.id );
        ]
      in
      Stdlib.List.flatten [ path_Home; path_Echo; path_Hello; path_About ]

    let _ = routes

    let handle =
      let router =
        Ppx_deriving_router_runtime.Handle.make
          (let routes : (Dream.request -> packed Lwt.t) Routes.route list =
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
       | About _x__002_ -> witness_About
       | Echo _x__003_ -> witness_Echo
       | Hello _x__004_ -> witness_Hello
        : t ->
          Ppx_deriving_router_runtime.response
          Ppx_deriving_router_runtime.Witness.t)

    let _ = witness
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
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

  include struct
    let _ = fun (_ : user) -> ()

    [@@@ocaml.warning "-39-11-27"]

    let rec user_of_json =
      (fun x ->
         match x with
         | `Assoc fs ->
             let x_id = ref Stdlib.Option.None in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "id" -> x_id := Stdlib.Option.Some (int_of_json v)
                   | name ->
                       Ppx_deriving_json_runtime.of_json_error
                         (Stdlib.Printf.sprintf "unknown field: %s" name));
                   iter fs
             in
             iter fs;
             {
               id =
                 (match Stdlib.( ! ) x_id with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Ppx_deriving_json_runtime.of_json_error
                       "missing field \"id\"");
             }
         | _ -> Ppx_deriving_json_runtime.of_json_error "expected a JSON object"
        : Yojson.Basic.t -> user)

    let _ = user_of_json

    [@@@ocaml.warning "-39-11-27"]

    let rec user_to_json =
      (fun x ->
         match x with { id = x_id } -> `Assoc [ ("id", int_to_json x_id) ]
        : user -> Yojson.Basic.t)

    let _ = user_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  type _ t =
    | List_users : user list t [@GET "/"]
    | Create_user : user t [@POST "/"]
    | Get_user : { id : int; active : bool } -> user t [@GET "/:id"]
    | Raw : Ppx_deriving_router_runtime.response t [@GET "/raw"]
  [@@deriving router]

  include struct
    let _ = fun (_ : _ t) -> ()

    let href (type a__055_) : a__055_ t -> string = function
      | List_users -> "/"
      | Create_user -> "/"
      | Get_user _x__056_ ->
          let id = _x__056_.id and active = _x__056_.active in
          let out__057_ = Buffer.create 16 in
          Buffer.add_char out__057_ '/';
          Ppx_deriving_router_runtime.Encode.encode_path out__057_
            (int_to_url_path id);
          let _sep__058_ = ref '?' in
          Stdlib.List.iter
            (fun (name, value) ->
              Buffer.add_char out__057_ !_sep__058_;
              Ppx_deriving_router_runtime.Encode.encode_query_key out__057_ name;
              _sep__058_ := '&';
              Buffer.add_char out__057_ '=';
              Ppx_deriving_router_runtime.Encode.encode_query_value out__057_
                value)
            (bool_to_url_query "active" active);
          Buffer.contents out__057_
      | Raw ->
          let out__059_ = Buffer.create 16 in
          Buffer.add_char out__059_ '/';
          Ppx_deriving_router_runtime.Encode.encode_path out__059_ "raw";
          Buffer.contents out__059_

    let _ = href

    let http_method (type a__050_) :
        a__050_ t -> [ `GET | `POST | `PUT | `DELETE ] = function
      | List_users -> `GET
      | Create_user -> `POST
      | Get_user _x__053_ -> `GET
      | Raw -> `GET

    let _ = http_method

    let body (type a__045_) :
        a__045_ t -> Ppx_deriving_router_runtime.json option = function
      | List_users -> None
      | Create_user -> None
      | Get_user _x__048_ -> None
      | Raw -> None

    let _ = body

    let encode_response (type a__040_) :
        a__040_ t -> a__040_ -> Ppx_deriving_router_runtime.json =
     fun route _value ->
      match route with
      | List_users -> (list_to_json user_to_json) _value
      | Create_user -> user_to_json _value
      | Get_user _x__043_ -> user_to_json _value
      | Raw -> failwith "response cannot be serialized to json"

    let _ = encode_response

    type packed =
      | Packed :
          'value t * 'value Ppx_deriving_router_runtime.Handle.encode
          -> packed

    type handler = {
      f :
        'value.
        'value t ->
        Ppx_deriving_router_runtime.request ->
        'value Ppx_deriving_router_runtime.return Lwt.t;
    }

    let routes =
      let path_Create_user =
        [
          Ppx_deriving_router_runtime.Handle.Route
            ( Routes.nil,
              (fun (_req__039_ : Ppx_deriving_router_runtime.request) ->
                match
                  Ppx_deriving_router_runtime.Request.method_ _req__039_
                with
                | `GET ->
                    let __url_query =
                      Ppx_deriving_router_runtime.Request.queries _req__039_
                    in
                    Lwt.return
                      (Packed
                         ( List_users,
                           Ppx_deriving_router_runtime.Handle.Encode_json
                             (list_to_json user_to_json) ))
                | `POST ->
                    let __url_query =
                      Ppx_deriving_router_runtime.Request.queries _req__039_
                    in
                    Lwt.return
                      (Packed
                         ( Create_user,
                           Ppx_deriving_router_runtime.Handle.Encode_json
                             user_to_json ))
                | _ ->
                    raise Ppx_deriving_router_runtime.Handle.Method_not_allowed),
              Stdlib.Fun.id );
        ]
      and path_Raw =
        [
          Ppx_deriving_router_runtime.Handle.Route
            ( Routes.( /? ) (Routes.s "raw") Routes.nil,
              (fun (_req__038_ : Ppx_deriving_router_runtime.request) ->
                match
                  Ppx_deriving_router_runtime.Request.method_ _req__038_
                with
                | `GET ->
                    let __url_query =
                      Ppx_deriving_router_runtime.Request.queries _req__038_
                    in
                    Lwt.return
                      (Packed
                         (Raw, Ppx_deriving_router_runtime.Handle.Encode_raw))
                | _ ->
                    raise Ppx_deriving_router_runtime.Handle.Method_not_allowed),
              Stdlib.Fun.id );
        ]
      and path_Get_user =
        [
          Ppx_deriving_router_runtime.Handle.Route
            ( Routes.( /? )
                (Routes.pattern
                   (fun x ->
                     let x = int_to_url_path x in
                     let buf = Buffer.create (String.length x) in
                     Ppx_deriving_router_runtime.Encode.encode_path buf x;
                     Buffer.contents buf)
                   (fun x ->
                     let x = Ppx_deriving_router_runtime.Decode.decode_path x in
                     int_of_url_path x)
                   "id")
                Routes.nil,
              (fun _param0 (_req__037_ : Ppx_deriving_router_runtime.request) ->
                match
                  Ppx_deriving_router_runtime.Request.method_ _req__037_
                with
                | `GET ->
                    let __url_query =
                      Ppx_deriving_router_runtime.Request.queries _req__037_
                    in
                    Lwt.return
                      (Packed
                         ( Get_user
                             {
                               id = _param0;
                               active =
                                 (match
                                    bool_of_url_query "active" __url_query
                                  with
                                 | Ok v -> v
                                 | Error err ->
                                     raise
                                       (Ppx_deriving_router_runtime.Handle
                                        .Invalid_query_parameter
                                          ("active", err)));
                             },
                           Ppx_deriving_router_runtime.Handle.Encode_json
                             user_to_json ))
                | _ ->
                    raise Ppx_deriving_router_runtime.Handle.Method_not_allowed),
              Stdlib.Fun.id );
        ]
      in
      Stdlib.List.flatten [ path_Get_user; path_Raw; path_Create_user ]

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
    let handle = handle
    let _ = handle

    open struct
      let (witness_List_users : user list Ppx_deriving_router_runtime.Witness.t)
          =
        Ppx_deriving_router_runtime.Witness.create ()

      let _ = witness_List_users

      let (witness_Create_user : user Ppx_deriving_router_runtime.Witness.t) =
        Ppx_deriving_router_runtime.Witness.create ()

      let _ = witness_Create_user

      let (witness_Get_user : user Ppx_deriving_router_runtime.Witness.t) =
        Ppx_deriving_router_runtime.Witness.create ()

      let _ = witness_Get_user

      let (witness_Raw :
            Ppx_deriving_router_runtime.response
            Ppx_deriving_router_runtime.Witness.t) =
        Ppx_deriving_router_runtime.Witness.create ()

      let _ = witness_Raw
    end

    let witness (type a__032_) :
        a__032_ t -> a__032_ Ppx_deriving_router_runtime.Witness.t = function
      | List_users -> witness_List_users
      | Create_user -> witness_Create_user
      | Get_user _x__035_ -> witness_Get_user
      | Raw -> witness_Raw

    let _ = witness
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
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
