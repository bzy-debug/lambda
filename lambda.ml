type expr =
  Var of string
| App of expr * expr
| Abs of string * expr

type value =
| Neutral of neutral
| Closure of env * string * expr
| Lazy of env * expr

and env = (string * value) list

and neutral =
  Nvar of string
| Napp of neutral * value


let f_string = Printf.sprintf

let rec expr_of_string = function
  | Var x -> x
  | App (r, d) -> (f_string "(%s %s)" (expr_of_string r) (expr_of_string d))
  | Abs (n, b) -> (f_string "(lambda (%s) %s)" n (expr_of_string b))

let id = Abs("x", Var("x"))
(* let () = print_endline (expr_of_string id) *)

let rec rec_fresh used var counter =
  let f_var = f_string "%s%d" var counter in
    if List.exists (String.equal f_var) used then
      rec_fresh used var (counter + 1)
    else
      f_var

let fresh used var =
  if List.exists (String.equal var) used then
    rec_fresh used var 0
  else
    var
(* let () = print_endline (fresh ["x"; "x0"; "x1"] "x") *)

exception Not_Closure

let rec evaluate env = function
  | Var x -> (
    (* print_endline "evaluate var"; *)
      try List.assoc x env with
        Not_found -> Neutral(Nvar(x))
    )
  | App (r, d) -> (
    (* print_endline "evaluate app"; *)
    let rv = evaluate env r in
      let dv = Lazy(env, d) in
        do_apply rv dv
    )
  | Abs (n, b) -> (
      (* print_endline "evaluate abs"; *)
      Closure(env, n, b)
    )

and do_apply rator rand =
  (* print_endline "do_apply"; *)
  match rator with
  | Closure (e, n, b) -> (evaluate ((n, rand) :: e) b)
  | Neutral n -> Neutral(Napp(n, rand))
  | Lazy (env, expr) -> do_apply (evaluate env expr) rand


let rec readback used v = match v with
  | Neutral n -> (
      (* print_endline "readback neutral"; *)
      match n with
      | Nvar nv -> Var(nv)
      | Napp (nr, nd) -> App(readback used (Neutral nr),
                             readback used nd)
    )
  | Closure (e, n, b) -> (
      (* print_endline "readback closure"; *)
      let fx = fresh (n :: used) n in
      (* let fx = fresh used n in *)
        Abs(fx,
        readback ([fx; n] @ used) (do_apply v (Neutral (Nvar fx))))
    )
  | Lazy (env, expr) -> readback used (evaluate env expr)

and normalize e = readback [] (evaluate [] e)

let ap = Abs("f", Abs("x", App(Var("f"), Var("x"))))
let varx = Var("x")
let test1 = App(ap, varx)

let tmp = Abs("x", App(Var "x", Var "x"))
let omega = App(tmp, tmp)
let rator = Abs("x", Abs("y", Var("y")))
let test2 = App (rator, omega)

let () = print_endline (expr_of_string (normalize test1))
let () = print_endline (expr_of_string (normalize test2))
let () = print_endline (expr_of_string (normalize id))

let zero = Abs("base", Abs("step", Var "base"))
let add1 = Abs("n", Abs("base", Abs("step", App(Var "step", App(App(Var "n", Var "base"), Var "step")))))
let iter = Abs("n", Abs("base", Abs("step", App(App(Var "n", Var "base"), Var "step"))))
let one = App(add1, zero)
let two = App(add1, one)
let three = App(add1, two)
let add = Abs("m", Abs("n", App(App(App(iter, Var "m"), Var "n"), add1)))

let () = print_endline (expr_of_string zero)
let () = print_endline (expr_of_string add1)
let () = print_endline (expr_of_string iter)
let () = print_endline (expr_of_string one)
let () = print_endline (expr_of_string (normalize one))
let () = print_endline (expr_of_string (normalize two))
let () = print_endline (expr_of_string (normalize three))
let () = print_endline (expr_of_string (normalize (App((App(add, two), two)))))

