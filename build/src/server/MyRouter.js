// Generated by Melange

import * as Curry from "melange.js/curry.js";
import * as Stdlib__String from "melange/string.js";
import * as React from "react";

var lastLocation = {
  contents: undefined
};

var lastUrl = {
  contents: undefined
};

function parsePathname(pathname) {
  if (pathname === "/") {
    return /* [] */0;
  } else {
    return Stdlib__String.split_on_char(/* '/' */47, pathname.slice(1, undefined));
  }
}

function getUrl(param) {
  var currentLocation = window.location.href;
  var match = lastUrl.contents;
  var match$1 = lastLocation.contents;
  if (match !== undefined && match$1 !== undefined && match$1 === currentLocation) {
    return match;
  }
  var parsedUrl = new URL(currentLocation);
  var pathname = parsedUrl.pathname;
  var url_path = parsePathname(pathname);
  var url_search = parsedUrl.search;
  var url = {
    path: url_path,
    search: url_search,
    pathname: pathname
  };
  lastLocation.contents = currentLocation;
  lastUrl.contents = url;
  return url;
}

function watchUrl(callback) {
  var onChange = function (param) {
    Curry._1(callback, undefined);
  };
  window.addEventListener("popstate", onChange);
  return function (param) {
    window.removeEventListener("popstate", onChange);
  };
}

function dispatchPopState(param) {
  window.dispatchEvent(new Event("popstate"));
}

function push(to_) {
  var history = window.history;
  var state = history.state;
  history.pushState(state, "", to_);
  dispatchPopState(undefined);
}

function replace(to_) {
  var history = window.history;
  var state = history.state;
  history.replaceState(state, "", to_);
}

function useUrl(param) {
  return React.useSyncExternalStore(watchUrl, getUrl);
}

export {
  lastLocation ,
  lastUrl ,
  parsePathname ,
  getUrl ,
  watchUrl ,
  dispatchPopState ,
  push ,
  replace ,
  useUrl ,
}
/* react Not a pure module */