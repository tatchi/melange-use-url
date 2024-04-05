type url = {
  path: list(string),
  search: string,
  // hash: string,
  pathname: string,
};

let lastLocation: ref(option(string)) = ref(None);
let lastUrl: ref(option(url)) = ref(None);

let parsePathname = (pathname: string) => {
  switch (pathname) {
  | "/" => []
  | pathname =>
    Js.String.slice(~start=1, pathname) |> String.split_on_char('/')
  };
};

let getUrl = () => {
  let currentLocation = Webapi.Dom.location |> Webapi.Dom.Location.href;

  switch (lastUrl^, lastLocation^) {
  | (Some(lastUrl), Some(lastLocation))
      when lastLocation === currentLocation => lastUrl
  | _ =>
    let parsedUrl = Webapi.Url.make(currentLocation);
    let pathname = Webapi.Url.pathname(parsedUrl);
    // let hash = Webapi.Url.hash(parsedUrl);

    let url: url = {
      path: parsePathname(pathname),
      search: Webapi.Url.search(parsedUrl),
      pathname,
    };

    lastLocation := Some(currentLocation);
    lastUrl := Some(url);

    url;
  };
};

let watchUrl = (callback: unit => unit) => {
  let onChange = (_: Dom.event) => {
    callback();
  };

  Webapi.Dom.window
  |> Webapi.Dom.Window.addEventListener("popstate", onChange);

  let unsubscribe = () =>
    Webapi.Dom.window
    |> Webapi.Dom.Window.removeEventListener("popstate", onChange);

  unsubscribe;
};

let dispatchPopState = () => {
  let _: bool =
    Webapi.Dom.window
    |> Webapi.Dom.Window.dispatchEvent(Webapi.Dom.Event.make("popstate"));
  ();
};

let push = (to_: string) => {
  let history = Webapi.Dom.history;

  let state = Webapi.Dom.History.state(history);

  history |> Webapi.Dom.History.pushState(state, "", to_);
  dispatchPopState();
};

let replace = (to_: string) => {
  let history = Webapi.Dom.history;

  let state = Webapi.Dom.History.state(history);

  history |> Webapi.Dom.History.replaceState(state, "", to_);
};

let useUrl = () => {
  let url =
    React.useSyncExternalStore(~subscribe=watchUrl, ~getSnapshot=getUrl);

  url;
};
