type point  = float * float
type vector = float list
type matrix = float list list

(*
Anywhere that a float*float is expected, you could use point, and vice-versa. 
The two are completely exchangeable for one another. In the following code, 
getx doesn't care whether you pass it a value that is annotated as one vs. the other:
*)

let getx : point -> float =
  fun (x,_) -> x

let pt : point = (1.,2.)
let floatpair : float*float = (1.,3.)

let one  = getx pt
let one' = getx floatpair