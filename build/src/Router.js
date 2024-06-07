// Generated by Melange

import * as Caml_option from "melange.js/caml_option.js";
import * as ReasonReactRouter from "reason-react/ReasonReactRouter.js";
import * as Routes from "../lib/routes/routes.js";
import * as Stdlib__Array from "melange/array.js";
import * as Stdlib__List from "melange/list.js";
import * as React from "react";
import * as JsxRuntime from "react/jsx-runtime";

function create(path, render) {
  return /* Route */{
          path: path,
          render: render
        };
}

function Router$Route(Props) {
  return null;
}

var context = React.createContext(undefined);

var make = context.Provider;

var Provider = {
  make: make
};

var Context = {
  context: context,
  Provider: Provider
};

var Route = {
  create: create,
  make: Router$Route,
  Context: Context
};

var Fragment = {};

function isFragment(element) {
  return element.type === React.Fragment;
}

var $$Element = {
  Fragment: Fragment,
  isFragment: isFragment
};

function Router$Routes(Props) {
  var children = Props.children;
  var fallbackOpt = Props.fallback;
  var fallback = fallbackOpt !== undefined ? Caml_option.valFromOption(fallbackOpt) : null;
  var match = React.useContext(context);
  var routes = [];
  React.Children.forEach(children, (function (element) {
          if (!React.isValidElement(element)) {
            return ;
          }
          if (element.type !== Router$Route) {
            console.error("Routes can only have <Route> children");
            return ;
          }
          var path = element.props.path;
          var render = element.props.render;
          var route = /* Route */{
            path: path,
            render: render
          };
          routes.push(route);
        }));
  var routes$1 = Routes.one_of(Stdlib__List.map((function (param) {
              return Routes.$at$neg$neg$great(param.path, param.render);
            }), Stdlib__Array.to_list(routes)));
  var el = Routes.match$p(routes$1, match.pathname);
  if (typeof el === "number") {
    return fallback;
  }
  if (el.TAG === /* FullMatch */0) {
    return el._0;
  }
  var parts = el._1;
  var value_pathname = Routes.Parts.wildcard_match(parts);
  var value_parent = match.parent + Routes.Parts.prefix(parts);
  var value = {
    pathname: value_pathname,
    parent: value_parent
  };
  return JsxRuntime.jsx(make, {
              value: value,
              children: el._0
            });
}

var Routes$1 = {
  $$Element: $$Element,
  make: Router$Routes
};

function Router$Provider(Props) {
  var children = Props.children;
  var url = ReasonReactRouter.useUrl(undefined, undefined);
  var path = url.path;
  var pathname = path ? Stdlib__List.fold_left((function (acc, v) {
            return acc + ("/" + v);
          }), "", path) : "/";
  return JsxRuntime.jsx(make, {
              value: {
                pathname: pathname,
                parent: ""
              },
              children: children
            });
}

var Provider$1 = {
  make: Router$Provider
};

export {
  Route ,
  Routes$1 as Routes,
  Provider$1 as Provider,
}
/* context Not a pure module */