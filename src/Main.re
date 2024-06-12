[%%mel.raw {|import "./index.css"|}];

module Client = {
  type root;

  [@mel.send] external render: (root, React.element) => unit = "render";

  [@mel.module "react-dom/client"]
  external createRoot: Dom.element => root = "createRoot";
};

module App = {
  [@react.component]
  let make = () => {
    <main> {React.string("app")} </main>;
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
