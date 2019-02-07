(* http://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/data/algebraic_data_types.html *)

let pi = 3.14

type point  = float * float
type vector = float list
type matrix = float list list

type day = Sun | Mon | Tue | Wed
         | Thu | Fri | Sat 

type ptype = TNormal | TFire | TWater

type peff = ENormal | ENotVery | Esuper

type shape =
  | Point  of point
  | Circle of point * float (* center and radius *)
  | Rect   of point * point (* lower-left and 
                               upper-right corners *)

let area = function
  | Point _ -> 0.0
  | Circle (_,r) -> pi *. (r ** 2.0)
  | Rect ((x1,y1),(x2,y2)) ->
      let w = x2 -. x1 in
      let h = y2 -. y1 in
        w *. h

let center = function
  | Point p -> p
  | Circle (p,_) -> p
  | Rect ((x1,y1),(x2,y2)) ->
      ((x2 -. x1) /. 2.0, 
       (y2 -. y1) /. 2.0)