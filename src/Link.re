[@react.component]
let make = (~href, ~children) => {
  open Router;
  let {Route.Context.parent, _} = React.useContext(Route.Context.context);

  let href = parent ++ "/" ++ href;

  <a
    href
    onClick={event => {
      React.Event.Mouse.preventDefault(event);
      ReasonReactRouter.push(href);
    }}>
    children
  </a>;
};
