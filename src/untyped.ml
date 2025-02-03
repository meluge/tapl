type term =
  | Var of string
  | Abs of string * term
  | App of term * term

let rec freeVar (t : term) : string list = 
  match t with 
  | Var x -> [x]
  | Abs (x, body) -> List.filter (fun v -> v <> x) (freeVar body)
  | App (t1, t2) -> List.append (freeVar t1) (freeVar t2)

let freshVar (existingVar : string) (varList : string list) : string =
  let rec newName i = 
    let candidate = existingVar ^ string_of_int i in
    if List.mem candidate varList then newName (i+1) else
      candidate 
    in
    newName 1

let rec alphaConv (og : string) (newVar : string) (t : term) : term =
  match t with
  | Var x -> if x = og then Var newVar else Var x
  | Abs (x, body) -> 
    if x = og then Abs (newVar, alphaConv og newVar body)
    else Abs (x, alphaConv og newVar body)
  | App (t1, t2) -> App(alphaConv og newVar t1, alphaConv og newVar t2)



let rec substitute (x : string) (s : term) (t : term) : term = 
  match t with
  | Var v -> if v = x then s else Var v 
  | Abs (v, body) ->
    if (v = x) then Abs (v, body)
    else if not (List.mem v (freeVar s)) 
      then Abs (v, substitute x s body)
    else let fresh = freshVar v (freeVar s @ freeVar body) in
      let newBody = alphaConv v fresh body in
      Abs (fresh, substitute x s newBody) 
  | App (t1, t2) -> App (substitute x s t1, substitute x s t2)


let rec betaRedLO t =
  match t with 
  | App (Abs (x, body), arg) -> substitute x arg body
  | App (t1, t2) -> App (betaRedLO t1, betaRedLO t2)
  | Abs (x, body) -> Abs (x, betaRedLO body)
  | Var _ -> t

let rec betaRedLOMult t =
  let t' = betaRedLO t in
  if t' = t then t
  else betaRedLOMult t'
(*Church Numerals*)

let toChurch (i : int) : term =
  let rec inner n =
    if n = 0 then Var "z"
    else App (Var "s", inner (n-1))
  in
  Abs ("s", (Abs ("z", (inner i))))

let fromChurch (t : term) : int =
  match t with
  | Abs (f, Abs (x, body)) ->
    let rec inner term =
      match term with
      | Var v when v = x -> 0
      | App (Var g, next) when g = f -> 1 + inner next
      | _ -> failwith "invalid"
    in
    inner body
  | _ -> failwith "invalid"

  let churchSucc =
    Abs("n", Abs("s", Abs("z",
      App(Var "s", App(App(Var "n", Var "s"), Var "z")))
    ))

