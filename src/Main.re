[%%mel.raw {|import "./index.css"|}];

module Client = {
  type root;

  [@mel.send] external render: (root, React.element) => unit = "render";

  [@mel.module "react-dom/client"]
  external createRoot: Dom.element => root = "createRoot";
};

module Link = {
  [@react.component]
  let make = (~href, ~children) =>
    <a
      href
      onClick={event => {
        React.Event.Mouse.preventDefault(event);
        ReasonReactRouter.push(href);
      }}>
      children
    </a>;
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
  let make = (~action) =>
    <div> {string_of_action(action)->React.string} </div>;
};

module User = {
  let routes = {
    let user_action = () => {
      let action =
        Routes.custom(
          ~serialize=UserAction.string_of_action,
          ~parse=UserAction.action_of_string,
          ~label=":action",
        );
      let path = () => Routes.(action /? nil);
      Routes.(path() @--> (action => <UserAction action />));
    };

    Routes.one_of([user_action()]);
  };

  [@react.component]
  let make = (~userId, ~rest, ~parentPrefix) => {
    let prefix = Routes.Parts.prefix(rest);
    let rest_url = Routes.Parts.wildcard_match(rest);
    <div>
      {("User id = " ++ string_of_int(userId))->React.string}
      <ul>
        <li>
          <Link href={parentPrefix ++ prefix ++ "/new"}>
            "New"->React.string
          </Link>
        </li>
        <li>
          <Link href={parentPrefix ++ prefix ++ "/edit"}>
            "Edit"->React.string
          </Link>
        </li>
      </ul>
      {switch (Routes.match'(routes, ~target=rest_url)) {
       | FullMatch(el)
       | MatchWithTrailingSlash(el) => el
       | NoMatch => React.null
       }}
    </div>;
  };
};

module Users = {
  let routes = parentPrefix => {
    let user_route = () => {
      let path = () => Routes.(int /? wildcard);
      Routes.(
        path() @--> ((userId, rest) => <User userId rest parentPrefix />)
      );
    };

    Routes.one_of([user_route()]);
  };

  [@react.component]
  let make = (~rest) => {
    let prefix = Routes.Parts.prefix(rest);
    let rest_url = Routes.Parts.wildcard_match(rest);
    <div>
      "Users"->React.string
      <ul>
        {{
           [|1, 2, 3|]
           |> Array.map(userId => {
                let userIdStr = string_of_int(userId);
                <li key=userIdStr>
                  <Link href={prefix ++ "/" ++ userIdStr}>
                    {("user " ++ userIdStr)->React.string}
                  </Link>
                </li>;
              });
         }
         ->React.array}
      </ul>
      {switch (Routes.match'(routes(prefix), ~target=rest_url)) {
       | FullMatch(el)
       | MatchWithTrailingSlash(el) => el
       | NoMatch => React.null
       }}
    </div>;
  };
};

module App = {
  let routes = {
    let root_route = () => {
      let path = () => Routes.(nil);
      Routes.(path() @--> <Root />);
    };
    let users_route = () => {
      let path = () => Routes.(s("users") /? wildcard);
      Routes.(path() @--> (rest => <Users rest />));
    };

    Routes.one_of([root_route(), users_route()]);
  };
  [@react.component]
  let make = () => {
    let url = ReasonReactRouter.useUrl();

    let pathname =
      switch (url.path) {
      | [] => "/"
      | path => path |> List.fold_left((acc, v) => acc ++ "/" ++ v, "")
      };
    <main>
      <nav>
        <ul>
          <li> <Link href="/"> "Root"->React.string </Link> </li>
          <li> <Link href="/users"> "Users"->React.string </Link> </li>
        </ul>
      </nav>
      {switch (Routes.match'(routes, ~target=pathname)) {
       | FullMatch(el)
       | MatchWithTrailingSlash(el) => el
       | NoMatch => <div> "Not found"->React.string </div>
       }}
    </main>;
  };
};

let node = ReactDOM.querySelector("#root");
switch (node) {
| None =>
  Js.Console.error("Failed to start React: couldn't find the #root element")
| Some(root) =>
  Client.(
    createRoot(root)->render(<React.StrictMode> <App /> </React.StrictMode>)
  )
};
