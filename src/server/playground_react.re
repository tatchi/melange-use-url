type route('v) =
  | Route(Routes.path('a, 'v), 'a): route('v);

let usePathname = () => {
  let url = ReasonReactRouter.useUrl();
  switch (url.path) {
  | [] => "/"
  | path => path |> List.fold_left((acc, v) => acc ++ "/" ++ v, "")
  };
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

module Root_router = {
  type t =
    | Home
    | Projects
    | SiteExplorer;

  // Generated begin
  let home = () => Routes.(nil);
  let projects = () => Routes.(s("projects") /? wildcard);
  let siteExplorer = () => Routes.(s("site-explorer") /? nil);

  let router =
    [
      Routes.(home() @--> Home),
      Routes.(projects() @--> (_ => Projects)),
      Routes.(siteExplorer() @--> SiteExplorer),
    ]
    |> Routes.one_of;

  let href = route => {
    switch (route) {
    | Home => Routes.sprintf(home())
    | Projects => Routes.sprintf(projects(), Routes.Parts.of_parts(""))
    | SiteExplorer => Routes.sprintf(siteExplorer())
    };
  };
  // Generated end
};

module Projects_router = {
  type t =
    | Home
    | Project_id({id: int});

  // Generated begin
  let home = () => Routes.(nil);
  let project_id = () => Routes.(int /? nil);

  let router =
    [
      Routes.(home() @--> Home),
      Routes.(project_id() @--> (id => Project_id({id: id}))),
    ]
    |> Routes.one_of;

  let href = route => {
    let prefix = Root_router.href(Projects);
    switch (route) {
    | Home => prefix ++ Routes.sprintf(home())
    | Project_id({id}) => prefix ++ Routes.sprintf(project_id(), id)
    };
  };
  // Generated end
};

module Project_detail_router = {
  type t =
    | Tasks
    | Milestones;

  // Generated begin
  let tasks = () => Routes.(s("tasks") /? nil);
  let milestones = () => Routes.(s("milestones") /? nil);

  let router =
    [Routes.(tasks() @--> Tasks), Routes.(milestones() @--> Milestones)]
    |> Routes.one_of;

  let href = (id, route) => {
    let prefix = Projects_router.href(Project_id({id: id}));
    switch (route) {
    | Tasks => prefix ++ Routes.sprintf(tasks())
    | Milestones => prefix ++ Routes.sprintf(milestones())
    };
  };
  // Generated end
};

module Project_detail = {
  let handle = route =>
    switch (route) {
    | Project_detail_router.Tasks => <div> {React.string("Tasks")} </div>
    | Milestones => <div> {React.string("Milestones")} </div>
    };

  [@react.component]
  let make = (~target) => {
    switch (Routes.match'(Project_detail_router.router, ~target)) {
    | Routes.NoMatch =>
      <div> {React.string(" Project_detail Not Found")} </div>
    | Routes.FullMatch(route)
    | Routes.MatchWithTrailingSlash(route) => handle(route)
    };
  };
};

module Projects_home = {
  [@react.component]
  let make = () => {
    <>
      {{
         [|1, 2, 3|]
         |> Array.map(id => {
              let idStr = string_of_int(id);
              <div key=idStr>
                <Link href={Projects_router.href(Project_id({id: id}))}>
                  {("Projects " ++ idStr)->React.string}
                </Link>
              </div>;
            });
       }
       ->React.array}
    </>;
  };
};

module Projects = {
  let handle = (route, ~rest) => {
    switch (route) {
    | Projects_router.Home => <Projects_home />
    | Project_id({id}) =>
      <>
        <h2> {React.string("Projects with id = " ++ string_of_int(id))} </h2>
        <div>
          <Link href={Project_detail_router.href(id, Tasks)}>
            {React.string("Tasks")}
          </Link>
        </div>
        <Link href={Project_detail_router.href(id, Milestones)}>
          {React.string("Milestones")}
        </Link>
        {switch (rest) {
         | "" => React.null
         | rest => <Project_detail target=rest />
         }}
      </>
    };
  };

  [@react.component]
  let make = (~target) => {
    let pathname = usePathname();
    switch (Routes.match'(Projects_router.router, ~target)) {
    | Routes.NoMatch =>
      <div> {React.string(" Project_router Not Found")} </div>
    | Routes.FullMatch(route)
    | Routes.MatchWithTrailingSlash(route) =>
      let matchedHref = Projects_router.href(route);
      let rest =
        Js.String.replace(~search=matchedHref, ~replacement="", pathname);
      handle(route, ~rest);
    };
  };
};

module Root = {
  let handle = (route, ~rest) => {
    switch (route) {
    | Root_router.Home => <h1> {React.string("Home")} </h1>
    | Projects =>
      <> <h1> {React.string("Projects")} </h1> <Projects target=rest /> </>
    | SiteExplorer => <h1> {React.string("SiteExplorer")} </h1>
    };
  };

  [@react.component]
  let make = () => {
    let pathname = usePathname();
    switch (Routes.match'(Root_router.router, ~target=pathname)) {
    | Routes.NoMatch => <div> {React.string("Root_router Not Found")} </div>
    | Routes.FullMatch(route)
    | Routes.MatchWithTrailingSlash(route) =>
      let matchedHref = Root_router.href(route);
      let rest =
        Js.String.replace(~search=matchedHref, ~replacement="", pathname);
      handle(route, ~rest);
    };
  };
};

module Client = {
  type root;

  [@mel.send] external render: (root, React.element) => unit = "render";

  [@mel.module "react-dom/client"]
  external createRoot: Dom.element => root = "createRoot";
};

module App = {
  [@react.component]
  let make = () => {
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
            href={Root_router.href(Projects)}
            style={ReactDOM.Style.make(~marginLeft="24px", ())}>
            "Projects"->React.string
          </Link>
          <Link
            href={Root_router.href(SiteExplorer)}
            style={ReactDOM.Style.make(~marginLeft="24px", ())}>
            "Site Explorer"->React.string
          </Link>
        </nav>
      </div>
      <main> <Root /> </main>
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
