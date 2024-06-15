// Generated by Melange

import * as Caml_option from "melange.js/caml_option.js";
import * as Curry from "melange.js/curry.js";
import * as Stdlib from "melange/stdlib.js";
import * as Stdlib__List from "melange/list.js";
import * as Stdlib__Map from "melange/map.js";
import * as Stdlib__Printf from "melange/printf.js";
import * as Stdlib__String from "melange/string.js";

function split_path(target) {
  var split_target = function (target) {
    switch (target) {
      case "" :
      case "/" :
          return /* [] */0;
      default:
        var xs = Stdlib__String.split_on_char(/* '/' */47, target);
        if (xs && xs.hd === "") {
          return xs.tl;
        } else {
          return xs;
        }
    }
  };
  var i = Stdlib__String.index_opt(target, /* '?' */63);
  if (i !== undefined) {
    if (i !== 0) {
      return split_target(Stdlib__String.sub(target, 0, i));
    } else {
      return /* [] */0;
    }
  } else {
    return split_target(target);
  }
}

var Util = {
  split_path: split_path
};

var Key = {};

var KeyMap = Stdlib__Map.Make({
      compare: Stdlib__String.compare
    });

var empty_children = KeyMap.empty;

var empty = {
  parsers: /* [] */0,
  children: empty_children,
  capture: undefined
};

function feed_params(t, params) {
  var _t = t;
  var _params = params;
  while(true) {
    var params$1 = _params;
    var t$1 = _t;
    var rs = t$1.parsers;
    var exit = 0;
    if (rs) {
      exit = 1;
    } else {
      if (!params$1) {
        return /* [] */0;
      }
      exit = 1;
    }
    if (exit === 1) {
      if (!params$1) {
        return rs;
      }
      var x = params$1.hd;
      var capture = t$1.capture;
      var exit$1 = 0;
      if (x === "") {
        if (!params$1.tl) {
          return rs;
        }
        exit$1 = 2;
      } else {
        exit$1 = 2;
      }
      if (exit$1 === 2) {
        var xs = params$1.tl;
        Curry._1(Stdlib__Printf.printf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "find_opt x = ",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Char_literal */12,
                        _0: /* '\n' */10,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "find_opt x = %s\n"
                }), x);
        var m$p = Curry._2(KeyMap.find_opt, x, t$1.children);
        if (m$p !== undefined) {
          Curry._1(Stdlib__Printf.printf(/* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* '[' */91,
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: "] FOUND!\n",
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "[%s] FOUND!\n"
                  }), x);
          _params = xs;
          _t = m$p;
          continue ;
        }
        Curry._1(Stdlib__Printf.printf(/* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* '[' */91,
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: "] not found, let's see capture\n",
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "[%s] not found, let's see capture\n"
                }), x);
        if (capture !== undefined) {
          Curry._1(Stdlib__Printf.printf(/* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* '[' */91,
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: "] capture  found\n",
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "[%s] capture  found\n"
                  }), x);
          _params = xs;
          _t = capture[1];
          continue ;
        }
        Curry._1(Stdlib__Printf.printf(/* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* '[' */91,
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: "] capture not found\n",
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "[%s] capture not found\n"
                }), x);
        return /* [] */0;
      }
      
    }
    
  };
}

function feed_params_2(t, params) {
  var _t = t;
  var _params = params;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var params$1 = _params;
    var t$1 = _t;
    if (!params$1) {
      return acc;
    }
    if (params$1.hd === "" && !params$1.tl) {
      return acc;
    }
    var xs = params$1.tl;
    var x = params$1.hd;
    var capture = t$1.capture;
    Curry._1(Stdlib__Printf.printf(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "find_opt x = ",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* '\n' */10,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "find_opt x = %s\n"
            }), x);
    var m$p = Curry._2(KeyMap.find_opt, x, t$1.children);
    if (m$p !== undefined) {
      var p = m$p.parsers;
      Curry._1(Stdlib__Printf.printf(/* Format */{
                _0: {
                  TAG: /* Char_literal */12,
                  _0: /* '[' */91,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: "] FOUND!\n",
                      _1: /* End_of_format */0
                    }
                  }
                },
                _1: "[%s] FOUND!\n"
              }), x);
      _acc = Stdlib.$at(p, acc);
      _params = xs;
      _t = m$p;
      continue ;
    }
    Curry._1(Stdlib__Printf.printf(/* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* '[' */91,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: "] not found, let's see capture\n",
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "[%s] not found, let's see capture\n"
            }), x);
    if (capture !== undefined) {
      var t$p = capture[1];
      Curry._1(Stdlib__Printf.printf(/* Format */{
                _0: {
                  TAG: /* Char_literal */12,
                  _0: /* '[' */91,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: "] capture  found\n",
                      _1: /* End_of_format */0
                    }
                  }
                },
                _1: "[%s] capture  found\n"
              }), x);
      _acc = Stdlib.$at(t$p.parsers, acc);
      _params = xs;
      _t = t$p;
      continue ;
    }
    Curry._1(Stdlib__Printf.printf(/* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* '[' */91,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: "] capture not found\n",
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "[%s] capture not found\n"
            }), x);
    return /* [] */0;
  };
}

function add(k, v, t) {
  var aux = function (k, t) {
    if (!k) {
      return {
              parsers: {
                hd: v,
                tl: t.parsers
              },
              children: t.children,
              capture: t.capture
            };
    }
    var capture = t.capture;
    var children = t.children;
    var r = k.tl;
    var x = k.hd;
    if (x.TAG === /* Match */0) {
      var w = x._0;
      var v$1 = Curry._2(KeyMap.find_opt, w, children);
      var t$p = v$1 !== undefined ? v$1 : empty;
      var t$p$p = aux(r, t$p);
      return {
              parsers: t.parsers,
              children: Curry._3(KeyMap.add, w, t$p$p, children),
              capture: t.capture
            };
    }
    var t$p$1 = capture !== undefined ? capture[1] : empty;
    var t$p$p$1 = aux(r, t$p$1);
    return {
            parsers: t.parsers,
            children: t.children,
            capture: [
              x._0,
              t$p$p$1
            ]
          };
  };
  return aux(k, t);
}

function $$int(name, r) {
  return {
          TAG: /* Conv */1,
          _0: {
            to_: (function (prim) {
                return String(prim);
              }),
            from_: Stdlib.int_of_string_opt,
            label: Curry._1(Stdlib__Printf.sprintf(/* Format */{
                      _0: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: ":int",
                          _1: /* End_of_format */0
                        }
                      },
                      _1: "%s:int"
                    }), name)
          },
          _1: r
        };
}

function str(name, r) {
  return {
          TAG: /* Conv */1,
          _0: {
            to_: (function (prim) {
                return prim;
              }),
            from_: (function (x) {
                return x;
              }),
            label: Curry._1(Stdlib__Printf.sprintf(/* Format */{
                      _0: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: ":string",
                          _1: /* End_of_format */0
                        }
                      },
                      _1: "%s:string"
                    }), name)
          },
          _1: r
        };
}

function s(w, r) {
  return {
          TAG: /* Match */0,
          _0: w,
          _1: r
        };
}

function route(r, handler) {
  return /* Route */{
          _0: r,
          _1: handler
        };
}

function $slash(m1, m2, r) {
  return Curry._1(m1, Curry._1(m2, r));
}

function $slash$question(m1, m2) {
  return Curry._1(m1, m2);
}

function route_pattern(param) {
  if (typeof param === "number") {
    return /* [] */0;
  } else if (param.TAG === /* Match */0) {
    return {
            hd: {
              TAG: /* Match */0,
              _0: param._0
            },
            tl: route_pattern(param._1)
          };
  } else {
    return {
            hd: {
              TAG: /* Capture */1,
              _0: param._0.label
            },
            tl: route_pattern(param._1)
          };
  }
}

function one_of(routes) {
  var routes$1 = Stdlib__List.rev(routes);
  return Stdlib__List.fold_left((function (routes, route) {
                var patterns = route_pattern(route._0);
                return add(patterns, route, routes);
              }), empty, routes$1);
}

function add_route(route, router) {
  var patterns = route_pattern(route._0);
  return add(patterns, route, router);
}

function parse_route(path, handler, target) {
  var _path = path;
  var _handler = handler;
  var _target = target;
  while(true) {
    var target$1 = _target;
    var handler$1 = _handler;
    var path$1 = _path;
    if (typeof path$1 === "number") {
      return {
              hd: handler$1,
              tl: /* [] */0
            };
    }
    if (path$1.TAG === /* Match */0) {
      if (!target$1) {
        return /* [] */0;
      }
      if (target$1.hd !== path$1._0) {
        return /* [] */0;
      }
      _target = target$1.tl;
      _path = path$1._1;
      continue ;
    }
    if (!target$1) {
      return /* [] */0;
    }
    var v = Curry._1(path$1._0.from_, target$1.hd);
    if (v === undefined) {
      return /* [] */0;
    }
    _target = target$1.tl;
    _handler = Curry._1(handler$1, Caml_option.valFromOption(v));
    _path = path$1._1;
    continue ;
  };
}

function match_routes(target, _routes, _acc) {
  while(true) {
    var acc = _acc;
    var routes = _routes;
    if (!routes) {
      return acc;
    }
    var rs = routes.tl;
    var match = routes.hd;
    var r = parse_route(match._0, match._1, target);
    if (r) {
      Stdlib__Printf.printf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[match_routes]: Got a match\n",
              _1: /* End_of_format */0
            },
            _1: "[match_routes]: Got a match\n"
          });
      _acc = Stdlib.$at(r, acc);
      _routes = rs;
      continue ;
    }
    Stdlib__Printf.printf(/* Format */{
          _0: {
            TAG: /* String_literal */11,
            _0: "[match_routes]: No match\n",
            _1: /* End_of_format */0
          },
          _1: "[match_routes]: No match\n"
        });
    _routes = rs;
    continue ;
  };
}

function match$p(router, target) {
  var target$1 = split_path(target);
  var routes = feed_params_2(router, target$1);
  var res = match_routes(target$1, routes, /* [] */0);
  if (res) {
    return /* FullMatch */{
            _0: res
          };
  } else {
    return /* NoMatch */0;
  }
}

function sprintf(t) {
  var k = function (x) {
    return x;
  };
  var k$1 = function (x) {
    return Curry._1(k, "/" + Stdlib__String.concat("/", x));
  };
  var aux = function (_k, _param) {
    while(true) {
      var param = _param;
      var k = _k;
      if (typeof param === "number") {
        return Curry._1(k, /* [] */0);
      }
      if (param.TAG === /* Match */0) {
        var w = param._0;
        _param = param._1;
        _k = (function(k,w){
        return function (s) {
          return Curry._1(k, {
                      hd: w,
                      tl: s
                    });
        }
        }(k,w));
        continue ;
      }
      var fmt = param._1;
      var to_ = param._0.to_;
      return (function(k,to_,fmt){
      return function (x) {
        return aux((function (rest) {
                      return Curry._1(k, {
                                  hd: Curry._1(to_, x),
                                  tl: rest
                                });
                    }), fmt);
      }
      }(k,to_,fmt));
    };
  };
  return aux(k$1, t);
}

var PatternTrie = {
  Key: Key,
  KeyMap: KeyMap,
  empty: empty,
  add: add,
  feed_params: feed_params,
  feed_params_2: feed_params_2
};

var empty_router = empty;

var nil = /* End */0;

var $at$neg$neg$great = route;

export {
  Util ,
  PatternTrie ,
  empty_router ,
  $$int ,
  str ,
  s ,
  nil ,
  $slash ,
  $slash$question ,
  $at$neg$neg$great ,
  route ,
  one_of ,
  route_pattern ,
  add_route ,
  match$p ,
  sprintf ,
}
/* KeyMap Not a pure module */