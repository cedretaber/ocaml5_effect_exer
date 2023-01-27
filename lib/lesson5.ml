
module type Scheduler = sig
  val async : (unit -> 'a) -> unit
  val yield : unit -> unit
  val run : (unit -> 'a) -> unit
end

module Scheduler : Scheduler = struct
  type _ Effect.t +=
    | Async : (unit -> 'a) -> unit Effect.t
    | Yield : unit Effect.t
  
  let async f = Effect.perform (Async f)
  let yield () = Effect.perform Yield

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q ()

  let rec run : 'a. (unit -> 'a) -> unit = fun main ->
    let open Effect.Deep in
    match_with main () {
      retc= (fun _ -> dequeue ());
      exnc= (fun e -> raise e);
      effc= fun (type b) (eff : b Effect.t) ->
        match eff with
        | Async f -> Some (fun (k : (b, _) continuation) ->
            enqueue (continue k);
            run f
          )
        | Yield -> Some (fun (k: (b, _) continuation) ->
            enqueue (continue k);
            dequeue ()
          )
        | _ -> None
    }
end

let main () =
  let main () =
    let mk_task name () =
      Printf.printf "starting %s\n%!" name;
      Scheduler.yield ();
      Printf.printf "ending %s\n%!" name
    in
    Scheduler.async (mk_task "a");
    Scheduler.async (mk_task "b")
  in
  Scheduler.run main