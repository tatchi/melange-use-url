[@warning "-26"];

[%%mel.raw {|import "./index.css"|}];

module Client = {
  type root;

  [@mel.send] external render: (root, React.element) => unit = "render";

  [@mel.module "react-dom/client"]
  external createRoot: Dom.element => root = "createRoot";
};

module Route = {
  type t =
    | Route({
        path: Routes.path('a, React.element),
        render: 'a,
      })
      : t;

  let make = (~path, ~render) => Route({path, render});
  // [@react.component]
  // let make = (~path as _: Routes.path('a, React.element), ~render as _: 'a) => React.null;
};

module RouteContext = {
  type routeCtx = {
    pathname: string,
    parent: string,
  };

  let routeContext: React.Context.t(routeCtx) =
    React.createContext(Obj.magic());

  module Provider = {
    include React.Context;

    let make = React.Context.provider(routeContext);
  };
};

module RouteProvider = {
  [@react.component]
  let make = (~children) => {
    let url = ReasonReactRouter.useUrl();
    let pathname =
      switch (url.path) {
      | [] => "/"
      | path => path |> List.fold_left((acc, v) => acc ++ "/" ++ v, "")
      };

    <RouteContext.Provider value={RouteContext.pathname, parent: ""}>
      children
    </RouteContext.Provider>;
  };
};

module Link = {
  [@react.component]
  let make = (~href, ~children) => {
    let {RouteContext.parent, _} =
      React.useContext(RouteContext.routeContext);

    let href = parent ++ "/" ++ href;

    <a
      href
      onClick={event => {
        React.Event.Mouse.preventDefault(event);
        ReasonReactRouter.push(href);
      }}>
      children
    </a>;
  };
};

module My_Routes = {
  [@react.component]
  let make = (~routes: list(Route.t), ~fallback=React.null) => {
    let {RouteContext.pathname, parent} =
      React.useContext(RouteContext.routeContext);

    let routes =
      routes
      |> List.map((Route.Route({path, render})) =>
           Routes.(path @--> render)
         )
      |> Routes.one_of;

    let elementToRender =
      switch (Routes.match'(routes, ~target=pathname)) {
      | FullMatch(el) => el
      | PartialMatch(el, parts) =>
        let value = {
          RouteContext.pathname: Routes.Parts.wildcard_match(parts),
          parent: parent ++ Routes.Parts.prefix(parts),
        };
        <RouteContext.Provider value> el </RouteContext.Provider>;
      | NoMatch => fallback
      };
    elementToRender;
  };
};

module Root = {
  [@react.component]
  let make = () => <div> "Root"->React.string </div>;
};

module UserAction = {
  type action =
    | New
    | Edit;
  let action_of_string = s =>
    switch (Js.String.toLocaleLowerCase(s)) {
    | "new" => Some(New)
    | "edit" => Some(Edit)
    | _ => None
    };

  let string_of_action =
    fun
    | New => "new"
    | Edit => "edit";

  [@react.component]
  let make = (~action, ~userId) =>
    <div>
      {(string_of_action(action) ++ " " ++ string_of_int(userId))
       ->React.string}
    </div>;
};

module User = {
  let action =
    Routes.custom(
      ~serialize=UserAction.string_of_action,
      ~parse=UserAction.action_of_string,
      ~label=":action",
    );

  [@react.component]
  let make = (~userId) => {
    <div>
      {("User id = " ++ string_of_int(userId))->React.string}
      <ul>
        <li> <Link href="new"> "New"->React.string </Link> </li>
        <li> <Link href="edit"> "Edit"->React.string </Link> </li>
      </ul>
      <My_Routes
        routes=[
          Route.make(~path=Routes.(action /? nil), ~render=action =>
            <UserAction action userId />
          ),
        ]
      />
    </div>;
  };
};
module Users = {
  [@react.component]
  let make = () => {
    <div>
      "Users"->React.string
      <ul>
        {{
           [|1, 2, 3|]
           |> Array.map(userId => {
                let userIdStr = string_of_int(userId);
                <li key=userIdStr>
                  <Link href=userIdStr>
                    {("user " ++ userIdStr)->React.string}
                  </Link>
                </li>;
              });
         }
         ->React.array}
      </ul>
      <My_Routes
        routes=[
          Route.make(~path=Routes.(int /? wildcard), ~render=(userId, _) =>
            <User userId />
          ),
        ]
      />
    </div>;
  };
};

module App = {
  [@react.component]
  let make = () => {
    <main>
      <nav>
        <ul>
          <li> <Link href=""> "Root"->React.string </Link> </li>
          <li> <Link href="users"> "Users"->React.string </Link> </li>
        </ul>
      </nav>
      <My_Routes
        fallback={<div> "No match"->React.string </div>}
        routes=[
          Route.make(~path=Routes.nil, ~render=<Root />),
          Route.make(~path=Routes.(s("users") /? wildcard), ~render=_ =>
            <Users />
          ),
        ]
      />
    </main>;
  };
};

let node = ReactDOM.querySelector("#root");
switch (node) {
| None =>
  Js.Console.error("Failed to start React: couldn't find the #root element")
| Some(root) =>
  Client.(
    createRoot(root)
    ->render(
        <React.StrictMode>
          <RouteProvider> <App /> </RouteProvider>
        </React.StrictMode>,
      )
  )
};
