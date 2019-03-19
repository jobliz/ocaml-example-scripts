(* https://news.ycombinator.com/item?id=9292838 *)
(* https://blog.janestreet.com/why-gadts-matter-for-performance/ *)

(* Original type: *)
type 'a t = | Array of 'a array
            | Bytes of bytes

(* GADT'd (same functionality as before): *)
type 'a t = | Array : 'a array -> 'a t
            | Bytes : bytes -> 'a t

(* Now using power of GADT to add constraint: *)
type 'a t = | Array : 'a array -> 'a t
            | Bytes : bytes -> char t
