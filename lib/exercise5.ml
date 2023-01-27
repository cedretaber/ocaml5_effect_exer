
module type Scheduler = sig
  type 'a promise
  val async : (unit -> 'a) -> 'a promise
  val await : 'a promise -> 'a
  val yield : unit -> unit
  val run : (unit -> 'a) -> unit
end

module Scheduler : Scheduler = struct

  open Effect
  open Deep

  type 'a _promise =
    | Waiting of ('a, unit) continuation list
    | Done of 'a
  type 'a promise = 'a _promise ref

  type _ Effect.t +=
    | Async : (unit -> 'a) -> ('a promise) Effect.t
    | Await : 'a promise -> 'a Effect.t
    | Yield : unit Effect.t
  
  let async f = perform (Async f)
  let await p = perform (Await p)
  let yield () = perform Yield

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q ()

  let run main =
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit = fun ps main ->
      match_with main () {
        retc= (fun v ->
          match !ps with
          | Done _ -> failwith "Trying to resolve already resolved promise"
          | Waiting pl ->
            ps := Done v;
            let s w = enqueue (fun () -> continue w v) in
            List.iter s pl;
            dequeue ()
        );
        exnc= (fun e -> raise e);
        effc= fun (type b) (eff : b Effect.t) ->
          match eff with
          | Async f -> Some (fun (k : (b, _) continuation) ->
            let np = ref (Waiting []) in
            enqueue (fun () -> fork np f);
            enqueue (fun () -> continue k np);
            dequeue ()
          )
          | Await p -> Some (fun (k : (b, _) continuation) ->
            begin match !p with
            | Done v ->
              enqueue (fun () -> continue k v);
            | Waiting ks ->
              p := Waiting (k :: ks);
            end;
            dequeue ()
          )
          | Yield -> Some (fun (k: (b, _) continuation) ->
            enqueue (continue k);
            dequeue ()
          )
          | _ -> None
      } in
    fork (ref (Waiting [])) main
end

open Scheduler

let main () =
  let main () =
    let task name () =
      Printf.printf "starting %s\n%!" name;
      let v = Random.int 100 in
      Printf.printf "yielding %s\n%!" name;
      yield ();
      Printf.printf "ending %s with %d\n%!" name v;
      v
    in
    let pa = async (task "a") in
    let pb = async (task "b") in
    let pc = async (fun () -> await pa + await pb) in
    Printf.printf "Sum is %d\n" (await pc);
    assert (await pa + await pb = await pc)
  in
  run main