module Time : sig
  type t
  val now : unit -> t
  val mk : int -> int -> int -> int -> int -> int -> t
  val to_string : t -> string
end = struct
  type t = Unix.tm

  let now () = Unix.localtime @@ Unix.time ()
  
  let mk year month tm_mday tm_hour tm_min tm_sec =
    let t = Unix.{
      tm_year= year - 1900; tm_mon= month - 1; tm_mday; tm_hour; tm_min; tm_sec;
      tm_wday= 0; tm_yday=0; tm_isdst= false
    } in
    let (_, t) = Unix.mktime t in
    t

  let to_string Unix.{tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _} =
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (1900 + tm_year) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec
end

open Effect
open Deep

type _ Effect.t += Now : Time.t Effect.t

let now () = perform Now

module Make(S: sig
  val now : unit -> Time.t
end) = struct
  let run f arg =
    try_with f arg {
      effc= fun (type c) (eff : c Effect.t) ->
        match eff with
        | Now -> Some (fun (k : (c, _) continuation) -> continue k (S.now ()))
        | _ -> None
    }
end

module Runner = Make(struct
  let now = Time.now
end)

module TestRunner = Make(struct
  let now () = Time.mk 2023 4 1 12 0 0
end)

let main () =
  let main () =
    let t = now () in
    Printf.printf "CurrentTime: %s\n" @@ Time.to_string t
  in
  Runner.run main ();
  TestRunner.run main ()
