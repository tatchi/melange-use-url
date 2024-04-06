module Route = {
  type t =
    | Route({
        path: Routes.path('a, React.element),
        render: 'a,
      })
      : t;

  let create = (~path, ~render) => Route({path, render});

  [@react.component]
  let make = (~path as _: Routes.path('a, React.element), ~render as _: 'a) => React.null;

  module Context = {
    type t = {
      pathname: string,
      parent: string,
    };

    let context: React.Context.t(t) = React.createContext(Obj.magic());

    module Provider = {
      include React.Context;

      let make = React.Context.provider(context);
    };
  };
};

module Routes = {
  [@mel.module "react"]
  external isValidElement: React.element => bool = "isValidElement";

  module Element = {
    type t = React.element;

    type elementType;

    module Fragment = {
      [@mel.module "react"] external type_: elementType = "Fragment";
    };

    [@mel.get] external type_: React.element => elementType = "type";

    let isFragment: React.element => bool =
      element => type_(element) === Fragment.type_;

    [@mel.get] [@mel.scope "props"]
    external path: React.element => Routes.path('a, React.element) = "path";
    [@mel.get] [@mel.scope "props"]
    external render: React.element => 'a = "render";
  };
  [@react.component]
  let make = (~children, ~fallback=React.null) => {
    let {Route.Context.pathname, parent} =
      React.useContext(Route.Context.context);
    let routes: Js.Array.t(Route.t) = [||];

    React.Children.forEach(children, element =>
      if (isValidElement(element)) {
        if (Element.type_(element) !== Obj.magic(Route.make)) {
          Js.Console.error("Routes can only have <Route> children");
        } else {
          let path = Element.path(element);
          let render = Element.render(element);
          let route = Route.create(~path, ~render);

          let _ = Js.Array.push(routes, ~value=route);
          ();
        };
      }
    );
    let routes =
      routes
      |> Array.to_list
      |> List.map((Route.Route({path, render})) =>
           Routes.(path @--> render)
         )
      |> Routes.one_of;

    let elementToRender =
      switch (Routes.match'(routes, ~target=pathname)) {
      | FullMatch(el) => el
      | PartialMatch(el, parts) =>
        let value = {
          Route.Context.pathname: Routes.Parts.wildcard_match(parts),
          parent: parent ++ Routes.Parts.prefix(parts),
        };
        <Route.Context.Provider value> el </Route.Context.Provider>;
      | NoMatch => fallback
      };
    elementToRender;
  };
};

module Provider = {
  [@react.component]
  let make = (~children) => {
    let url = ReasonReactRouter.useUrl();
    let pathname =
      switch (url.path) {
      | [] => "/"
      | path => path |> List.fold_left((acc, v) => acc ++ "/" ++ v, "")
      };

    <Route.Context.Provider value={Route.Context.pathname, parent: ""}>
      children
    </Route.Context.Provider>;
  };
};
