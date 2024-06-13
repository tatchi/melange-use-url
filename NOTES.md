- We generate an extra `let _sep__017_ = ref '?'` for routes like `About`. They do
  not take any param so that's not needed

```ocaml
type t =
  | Home [@GET "/"]
  | About
  | Hello of { name : string; repeat : int option } [@GET "/hello/:name"]
  [@@deriving router]
```
