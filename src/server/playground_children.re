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

module Projects_pages = {
  type t =
    | Index
    | Project_id({id: int});

  // Generated begin
  let index = () => Routes.(nil);
  let project_id = () => Routes.(int /? nil);

  let router =
    [
      Routes.(index() @--> Index),
      Routes.(project_id() @--> (id => Project_id({id: id}))),
    ]
    |> Routes.one_of;

  let href = route => {
    switch (route) {
    | Index => Routes.sprintf(index())
    // | Id({id}) => Routes.sprintf(id(), Routes.Parts.of_parts(""))
    | Project_id({id}) => Routes.sprintf(project_id(), id)
    };
  };
  // Generated end
};

module Root_pages = {
  type t =
    | Home
    | Projects({children: option(Projects_pages.t)})
    | SiteExplorer;

  // Generated begin
  let home = () => Routes.(nil);
  let projects = () => Routes.(s("projects") /? wildcard);
  let siteExplorer = () => Routes.(s("site-explorer") /? nil);

  let router =
    [
      Routes.(home() @--> Home),
      Routes.(
        projects()
        @--> (
          parts => {
            let target = Routes.Parts.wildcard_match(parts);
            switch (Routes.match'(Projects_pages.router, ~target)) {
            | FullMatch(t)
            | MatchWithTrailingSlash(t) => Projects({children: Some(t)})
            | NoMatch => Projects({children: None})
            };
          }
        )
      ),
      Routes.(siteExplorer() @--> SiteExplorer),
    ]
    |> Routes.one_of;

  let href = route => {
    switch (route) {
    | Home => Routes.sprintf(home())
    | Projects({children}) =>
      Routes.sprintf(
        projects(),
        switch (children) {
        | None => Routes.Parts.of_parts("")
        | Some(children) =>
          Routes.Parts.of_parts(Projects_pages.href(children))
        },
      )
    | SiteExplorer => Routes.sprintf(siteExplorer())
    };
  };
  // Generated end
};

module Projects_index = {
  [@react.component]
  let make = () => {
    <>
      {{
         [|1, 2, 3|]
         |> Array.map(id => {
              let idStr = string_of_int(id);
              <div key=idStr>
                <Link
                  href={Root_pages.href(
                    Projects({
                      children: Some(Projects_pages.Project_id({id: id})),
                    }),
                  )}>
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
  let handle = route => {
    switch (route) {
    | Projects_pages.Index => <Projects_index />
    | Project_id({id}) =>
      <>
        <h2> {React.string("Projects with id = " ++ string_of_int(id))} </h2>
        // <div>
        //   <Link href={Project_detail_router.href(id, Tasks)}>
        //     {React.string("Tasks")}
        //   </Link>
        // </div>
        // <Link href={Project_detail_router.href(id, Milestones)}>
        //   {React.string("Milestones")}
        // </Link>
        // {switch (rest) {
        //  | "" => React.null
        //  | rest => <Project_detail target=rest />
        //  }}
      </>
    };
  };

  [@react.component]
  let make = (~target) => {
    switch (Routes.match'(Projects_pages.router, ~target)) {
    | Routes.NoMatch =>
      <div> {React.string(" Project_pages Not Found")} </div>
    | Routes.FullMatch(route)
    | Routes.MatchWithTrailingSlash(route) =>
      // let matchedHref = Projects_pages.href(route);
      // let rest =
      //   Js.String.replace(~search=matchedHref, ~replacement="", pathname);
      handle(route)
    };
  };
};

module Root = {
  let handle = route => {
    switch (route) {
    | Root_pages.Home => <h1> {React.string("Home")} </h1>
    | Projects({children}) =>
      let children =
        switch (children) {
        | None => React.null
        | Some(children) => Projects.handle(children)
        };

      <> <h1> {React.string("Projects")} </h1> children </>;
    | SiteExplorer => <h1> {React.string("SiteExplorer")} </h1>
    };
  };

  [@react.component]
  let make = () => {
    let pathname = usePathname();
    switch (Routes.match'(Root_pages.router, ~target=pathname)) {
    | Routes.NoMatch => <div> {React.string("Root_pages Not Found")} </div>
    | Routes.FullMatch(route)
    | Routes.MatchWithTrailingSlash(route) =>
      // let matchedHref = Root_pages.href(route);
      // let rest =
      //   Js.String.replace(~search=matchedHref, ~replacement="", pathname);
      handle(route)
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
          <Link href={Root_pages.href(Home)}> "Home"->React.string </Link>
          <Link
            href={Root_pages.href(Projects({children: None}))}
            style={ReactDOM.Style.make(~marginLeft="24px", ())}>
            "Projects"->React.string
          </Link>
          <Link
            href={Root_pages.href(SiteExplorer)}
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
