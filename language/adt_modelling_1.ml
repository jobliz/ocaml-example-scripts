(* https://fsharpforfunandprofit.com/posts/type-size-and-design/ *)

type 'a option = None | Some of 'a
type 'a sequence = Nil | Cons of 'a * 'a sequence

type direction = 
  | North
  | East
  | South
  | West

type speed = 
  | Slow
  | Fast

type velocity = {
  direction: direction;
  speed: speed 
}

type movement = 
  | Moving of direction
  | NotMoving

(* 
Here’s a real example:

    You have a website where some users are registered and some are not.
    For all users, you have a session id
    For registered users only, you have extra information

We could model that requirement like this:
*)

type userInfo = {name:string} 
type sessionId = SessionId of int

(* We could model that requirement like this: *)

type websiteUser_first = 
  | RegisteredUser of sessionId * userInfo
  | GuestUser of sessionId 

(* or alternatively, we can pull the common SessionId up to a higher level like this: *)

type websiteUserInfo_second = 
  | RegisteredUser of userInfo
  | GuestUser 

type websiteUser_second = {
  sessionId : sessionId;
  info: websiteUserInfo_second 
}

(*
Which is better? In one sense, they are both the “same”, but obviously the 
best design depends on the usage pattern.

- If you care more about the type of user than the session id, then 
  version 1 is better.
- If you are constantly looking at the session id without caring about
  the type of user, then version 2 is better.

The nice thing about knowing that they are isomorphic is that you can define
both types if you like, use them in different contexts, and losslessly map 
between them as needed.
*)




