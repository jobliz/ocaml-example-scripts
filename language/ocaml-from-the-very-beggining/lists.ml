(* chapter 6*)

let rec double l =
  match l with
    | [] -> []
    | h::t -> (h * 2) :: double t

let rec evens l =
  match l with
    | [] -> []
    | h::t -> (h mod 2 = 0) :: evens t

let rec map f l =
  match l with
    | [] -> []
    | h::t -> f h :: map f t

let rec mapl f l =
  match l with
  | [] -> []
  | h::t -> map f h :: mapl f t

let rec take n l =
  match l with
    | [] ->
      if n = 0
        then []
        else raise (Invalid_argument "take")
    | h::t ->
      if n < 0 then raise (Invalid_argument "take") else
        if n = 0 then [] else h :: take (n - 1) t

let rec drop n l =
  match l with
  | [] ->
    if n = 0
      then []
      else raise (Invalid_argument "drop")
  | h::t ->
    if n < 0 then raise (Invalid_argument "drop") else
      if n = 0 then l else drop (n - 1) t

let rec last l =
  match l with
  | [] -> raise Not_found
  | [x] -> x
  | _::t -> last t

