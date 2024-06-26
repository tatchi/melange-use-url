## Flat routes

```ocaml
type all_routes =
  | Home [@GET "/home"]
  | Projects [@GET "/projects"]
  | Project of { id : int } [@GET "/projects/:id"]
  | Poject_tasks of { id : int } [@GET "/projects/:id/tasks"]
[@@deriving router]
```

### Pros

- Explicit about the routes and their params (must take params from parent)

### Cons

- Can be annoying to repeat the parent path

I have a modified version of `routes` that return multiple matches for an url (target). It creates the parent - child relation automatically based on the path. It can be a bit annoying because we need to repeat the parent path when defining a child.
I thought it would work but I'm just realizing that it's not enough to rely on the path. For with this solution `projects/:id` would be a parent of `projects/:id/new` while it could be that we want them at the same level

## Children

```ocaml
type single_project_routes =
  | Project_tasks [@GET "tasks"]

type project_routes =
  | Project of { id : int, chilren : single_project_routes  } [@GET ":id"]
  | Project_new [@GET "new"]

type all_routes =
  | Home [@GET "/home"]
  | Projects of { children : project_routes } [@GET "/projects"]
[@@deriving router]
```

We'd need to handle the special `children` field in the ppx. And behind the scene, the ppx would have to modify (or rather output) new types. To get get of `children` + inject parent data to allow create `href` with all params + path info. Or we can let the user repeat them and validate at build time through the ppx?
Not sure how the matching would work. Each type could create a new router and we match at each level ?

```ocaml
type single_project_routes =
| Project_tasks { id : int  } [@GET "/projects/:id/tasks"]

type project_routes =
  | Project of { id : int  } [@GET "/projects/:id"]
  | Project_new [@GET "/projects/new"]

type all_routes =
  | Home [@GET "/home"]
  | Projects [@GET "/projects"]
[@@deriving router]
```

## Add support in routes itself

```ocaml

let home () = Routes.(s "home" / str)
let inner () = Routes.(s "dashoard" / int)

let both () = Routes.(home () /~ (inner () /? nil))
let res = Routes.(both () @--> fun a b -> "")
```

## Others

Maybe specify the children in the return value of the handler ? But then we don't know them ahead of time so we cannot generate `hrefs`

## Notes from call with AndreyPopp

```reason

module Dashboard_details = {
  type t =
  | Analytics [@GET "/analytics"]
  | Export [@GET "export"];
};

let dashboard_details_handler = (route, id) => switch {
  | Analytics => ...
}

module Dashboard = {
  type t =
  | Home [@get ""]
  | Details({id: int, children: Dashboard_details.t [@children]}) [@get ":id"]
};

let dashboard_handler = route => switch {
  | Details ({id}) => <Layout>{dashboard_details_handler(route, id)}</Layout>
  | Another ({id}) => <Layout>{dashboard_details_handler(route, id)}</Layout>
}

module Root = {
  type t(_) =
  | [@prefix "/dashboard"] Dashboard(Dashboard.t): t(React.element);
};
```
