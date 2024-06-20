type route('v) =
  | Route(Routes.path('a, 'v), 'a): route('v);

module Dashboard_router = {
  let home = () => Routes.(nil);
  let dashboard_id = () => Routes.(int /? nil);

  type t =
    // | Home
    | Dashboard_id({id: int});

  let router =
    [
      // Routes.(home() @--> Home),
      Routes.(dashboard_id() @--> (id => Dashboard_id({id: id}))),
    ]
    |> Routes.one_of;

  let href = route => {
    switch (route) {
    // | Home => Routes.sprintf(home())
    | Dashboard_id({id}) => Routes.sprintf(dashboard_id(), id)
    };
  };

  let handle = route =>
    switch (route) {
    // | Home => <h1> {React.string("Home")} </h1>
    | Dashboard_id({id}) =>
      <div>
        {React.string("dashboard with id = " ++ string_of_int(id))}
      </div>
    };

  [@react.component]
  let make = (~target) => {
    switch (Routes.match'(router, ~target)) {
    | Routes.NoMatch =>
      <div> {React.string(" Dashboard_router Not Found")} </div>
    | Routes.FullMatch(route)
    | Routes.MatchWithTrailingSlash(route) => handle(route)
    };
  };
};

module Root_router = {
  let home = () => Routes.(nil);
  let dashboard = () => Routes.(s("dashboard") /? nil);
  let siteExplorer = () => Routes.(s("site-explorer") /? nil);

  type t =
    | Home
    | Dashboard
    | SiteExplorer;

  let router =
    [
      Routes.(home() @--> Home),
      Routes.(dashboard() @--> Dashboard),
      Routes.(siteExplorer() @--> SiteExplorer),
    ]
    |> Routes.one_of;

  let href = route => {
    switch (route) {
    | Home => Routes.sprintf(home())
    | Dashboard => Routes.sprintf(dashboard())
    | SiteExplorer => Routes.sprintf(siteExplorer())
    };
  };

  let handle = route =>
    switch (route) {
    | Home => <h1> {React.string("Home")} </h1>
    | Dashboard =>
      <>
        <h1> {React.string("Dashboard")} </h1>
        <Dashboard_router target="" />
      </>
    | SiteExplorer => <h1> {React.string("SiteExplorer")} </h1>
    };
};

module Dashboard_page = {
  [@react.component]
  let make = () => {
    <>
      <h1> {React.string("Dashboard")} </h1>
      {{
         [|1, 2, 3|]
         |> Array.map(id => {
              let idStr = string_of_int(id);
              <div
                key=idStr
                // <Link href={Da}>
                //   {("Dashboard " ++ idStr)->React.string}
                // </Link>
              />;
            });
       }
       ->React.array}
    </>;
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
  let make = (~href, ~children, ~style=?) => {
    <a
      ?style
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
    <header>
      <div style={ReactDOM.Style.make(~padding="4px 5px", ())}>
        <nav
          style={ReactDOM.Style.make(
            ~display="flex",
            ~alignItems="center",
            (),
          )}>
          <Link href={Root_router.href(Home)}> "Home"->React.string </Link>
          <Link
            href={Root_router.href(Dashboard)}
            style={ReactDOM.Style.make(~marginLeft="24px", ())}>
            "Dashboard"->React.string
          </Link>
          <Link
            href={Root_router.href(SiteExplorer)}
            style={ReactDOM.Style.make(~marginLeft="24px", ())}>
            "Site Explorer"->React.string
          </Link>
        </nav>
      </div>
      <main>
        {switch (Routes.match'(Root_router.router, ~target=pathname)) {
         | Routes.NoMatch =>
           <div> {React.string("Root_router Not Found")} </div>
         | Routes.FullMatch(route)
         | Routes.MatchWithTrailingSlash(route) => Root_router.handle(route)
         }}
      </main>
    </header>;
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
