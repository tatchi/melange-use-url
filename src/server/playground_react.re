type route('v) =
  | Route(Routes.path('a, 'v), 'a): route('v);

let home = () => Routes.(s("home") / str /? nil);

let project = () => Routes.(s("project") / int /? nil);

type my_routes =
  | Home({name: string})
  | Project({id: int});

let router =
  [
    Routes.(home() @--> (name => Home({name: name}))),
    Routes.(project() @--> (id => Project({id: id}))),
  ]
  |> Routes.one_of;

let handle = route =>
  switch (route) {
  | Home({name}) => <div> {React.string("Welcome home " ++ name)} </div>
  | Project({id}) =>
    <div> {React.string("project id =  " ++ string_of_int(id))} </div>
  };

let href = route => {
  switch (route) {
  | Home({name}) => "/home/" ++ name
  | Project({id}) => "/project/" ++ string_of_int(id)
  };
};

module Client = {
  type root;

  [@mel.send] external render: (root, React.element) => unit = "render";

  [@mel.module "react-dom/client"]
  external createRoot: Dom.element => root = "createRoot";
};

module Link = {
  [@react.component]
  let make = (~href, ~children) => {
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

module App = {
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
          <li>
            <Link href={href(Home({name: "tatchi"}))}>
              "Home"->React.string
            </Link>
          </li>
          <li>
            <Link href={href(Project({id: 88}))}>
              "Project 88"->React.string
            </Link>
          </li>
        </ul>
      </nav>
      {switch (Routes.match'(router, ~target=pathname)) {
       | Routes.NoMatch => assert(false)
       | Routes.FullMatch(route)
       | Routes.MatchWithTrailingSlash(route) => handle(route)
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
