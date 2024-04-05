// Generated by Melange

import * as Caml_option from "melange.js/caml_option.js";
import * as ReasonReactRouter from "reason-react/ReasonReactRouter.js";
import * as Routes from "../lib/routes/routes.js";
import * as Stdlib__Array from "melange/array.js";
import * as Stdlib__List from "melange/list.js";
import * as React from "react";
import * as Client from "react-dom/client";
import * as JsxRuntime from "react/jsx-runtime";

import "./index.css"
;

var Client$1 = {};

function Main$Link(Props) {
  var href = Props.href;
  var children = Props.children;
  return JsxRuntime.jsx("a", {
              children: children,
              href: href,
              onClick: (function ($$event) {
                  $$event.preventDefault();
                  ReasonReactRouter.push(href);
                })
            });
}

var Link = {
  make: Main$Link
};

function Main$Root(Props) {
  return JsxRuntime.jsx("div", {
              children: "Root"
            });
}

var Root = {
  make: Main$Root
};

function Main$User(Props) {
  var userId = Props.userId;
  return JsxRuntime.jsx("div", {
              children: "User id = " + String(userId)
            });
}

var User = {
  make: Main$User
};

function user_route(param) {
  return Routes.$at$neg$neg$great(Routes.$slash$question(Routes.$$int, Routes.nil), (function (userId) {
                return JsxRuntime.jsx(Main$User, {
                            userId: userId
                          });
              }));
}

var routes = Routes.one_of({
      hd: user_route(undefined),
      tl: /* [] */0
    });

function Main$Users(Props) {
  var rest = Props.rest;
  var prefix = Routes.Parts.prefix(rest);
  var rest_url = Routes.Parts.wildcard_match(rest);
  var match = Routes.match$p(routes, rest_url);
  return JsxRuntime.jsxs("div", {
              children: [
                "Users",
                JsxRuntime.jsx("ul", {
                      children: Stdlib__Array.map((function (userId) {
                              var userIdStr = String(userId);
                              return JsxRuntime.jsx("li", {
                                          children: JsxRuntime.jsx(Main$Link, {
                                                href: prefix + ("/" + userIdStr),
                                                children: "user " + userIdStr
                                              })
                                        }, userIdStr);
                            }), [
                            1,
                            2,
                            3
                          ])
                    }),
                typeof match === "number" ? null : match._0
              ]
            });
}

var Users = {
  routes: routes,
  make: Main$Users
};

function root_route(param) {
  return Routes.$at$neg$neg$great(Routes.nil, JsxRuntime.jsx(Main$Root, {}));
}

function users_route(param) {
  return Routes.$at$neg$neg$great(Routes.$slash$question((function (param) {
                    return Routes.s("users", param);
                  }), Routes.wildcard), (function (rest) {
                return JsxRuntime.jsx(Main$Users, {
                            rest: rest
                          });
              }));
}

var routes$1 = Routes.one_of({
      hd: root_route(undefined),
      tl: {
        hd: users_route(undefined),
        tl: /* [] */0
      }
    });

function Main$App(Props) {
  var url = ReasonReactRouter.useUrl(undefined, undefined);
  var path = url.path;
  var pathname = path ? Stdlib__List.fold_left((function (acc, v) {
            return acc + ("/" + v);
          }), "", path) : "/";
  var match = Routes.match$p(routes$1, pathname);
  return JsxRuntime.jsxs("main", {
              children: [
                JsxRuntime.jsx("nav", {
                      children: JsxRuntime.jsxs("ul", {
                            children: [
                              JsxRuntime.jsx("li", {
                                    children: JsxRuntime.jsx(Main$Link, {
                                          href: "/",
                                          children: "Root"
                                        })
                                  }),
                              JsxRuntime.jsx("li", {
                                    children: JsxRuntime.jsx(Main$Link, {
                                          href: "/users",
                                          children: "Users"
                                        })
                                  })
                            ]
                          })
                    }),
                typeof match === "number" ? JsxRuntime.jsx("div", {
                        children: "Not found"
                      }) : match._0
              ]
            });
}

var App = {
  routes: routes$1,
  make: Main$App
};

var node = document.querySelector("#root");

if (node == null) {
  console.error("Failed to start React: couldn't find the #root element");
} else {
  Client.createRoot(node).render(JsxRuntime.jsx(React.StrictMode, {
            children: JsxRuntime.jsx(Main$App, {})
          }));
}

var node$1 = (node == null) ? undefined : Caml_option.some(node);

export {
  Client$1 as Client,
  Link ,
  Root ,
  User ,
  Users ,
  App ,
  node$1 as node,
}
/*  Not a pure module */
