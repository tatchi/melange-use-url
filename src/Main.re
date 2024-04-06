[@warning "-26"];

[%%mel.raw {|import "./index.css"|}];

module Client = {
  type root;

  [@mel.send] external render: (root, React.element) => unit = "render";

  [@mel.module "react-dom/client"]
  external createRoot: Dom.element => root = "createRoot";
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
      <Router.Routes>
        <Router.Route
          path=Routes.(action /? nil)
          render={action => <UserAction action userId />}
        />
      </Router.Routes>
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
      <Router.Routes>
        <Router.Route
          path=Routes.(int /? wildcard)
          render={(userId, _: Routes.Parts.t) => <User userId />}
        />
      </Router.Routes>
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
      <Router.Routes fallback={<div> "No match"->React.string </div>}>
        <Router.Route path=Routes.nil render={<Root />} />
        <Router.Route
          path=Routes.(s("users") /? wildcard)
          render={(_: Routes.Parts.t) => <Users />}
        />
      </Router.Routes>
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
          <Router.Provider> <App /> </Router.Provider>
        </React.StrictMode>,
      )
  )
};
