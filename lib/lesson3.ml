open Effect
open Effect.Deep

type _ Effect.t  += Peek : int Effect.t
                  | Poke : unit Effect.t

let a i = perform Peek + Random.int i
let b i = a i + Random.int i
let c i = b i + Random.int i
let d i =
  Random.int i +
  try_with c i {
    effc= fun (type a) (e : a t) ->
      match e with
      | Poke -> Some (fun (k : (a, _) continuation) -> continue k ())
      | _ -> None
  }
let e i =
  Random.int i +
  try_with d i {
    effc= fun (type a) (e : a t) ->
      match e with
      | Peek -> Some (fun (k : (a, _) continuation) ->
          Printexc.(print_raw_backtrace stdout (Effect.Deep.get_callstack k 100));
          flush stdout;
          continue k 42
        )
      | _ -> None
  }

let main () = Printf.printf "%d\n" (e 100)