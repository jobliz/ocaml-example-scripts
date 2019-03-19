(* http://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/ads/monads.html *)

(* general structure of a monad *)
module type Monad = sig
  type 'a t
  val return : 'a -> 'a t  
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* this works fine and evaluates to 3 *)
let x = 1 + (4 / 2)

let div (x:int) (y:int) : int option =
  if y=0 then None
  else Some (x / y)

let ( / ) = div

(* this won't type check *)
(* let x = 1 + (4 / 2) *)

let plus_opt (x:int option) (y:int option) : int option =
  match x, y with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Pervasives.( + ) a b)

let minus_opt (x:int option) (y:int option) : int option =
  match x,y with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Pervasives.( - ) a b)


let ( + ) = plus_opt
let ( - ) = minus_opt