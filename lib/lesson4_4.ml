type ('elt, 'container) iterator = ('elt -> unit) -> 'container -> unit
type 'elt generator = unit -> 'elt option

let generate (type elt) (iter : (elt, 'container) iterator) (container : 'container) : elt generator =
  let open Effect in
  let open Effect.Shallow in
  let module M = struct
    type _ Effect.t += Yield : elt -> unit Effect.t
  end in
  let open M in
  let f () =
    container |> iter (fun elm -> perform (Yield elm))
  in
  let cont = ref (fiber f) in
  let loop () = continue_with !cont () {
    retc= (fun _ -> None);
    exnc= (fun e -> raise e);
    effc= fun (type c) (eff : c Effect.t) ->
      match eff with
      | Yield elt -> Some (fun (k : (c, _) continuation) ->
          cont := k;
          Some elt
        )
      | _ -> None
  } in
  loop

let rec nats : int -> (int, unit) iterator = fun v f () ->
  f v;
  nats (v + 1) f ()

let gen_nats : int generator = generate (nats 0) ()

type 'a stream = unit -> 'a

let inf : 'a generator -> 'a stream = fun g () ->
  match g () with
  | Some e -> e
  | None -> failwith "Finite generator"

let rec filter : 'a stream -> ('a -> bool) -> 'a stream =
  fun g p () ->
    let v = g () in
    if p v then
      v
    else
      filter g p ()

let map : 'a stream -> ('a -> 'b) -> 'b stream =
  fun g f () -> f @@ g ()

let gen_primes =
  let s = inf (generate (nats 2) ()) in
  let rs = ref s in
  fun () ->
    let s = !rs in
    let prime = s () in
    rs := filter s (fun n -> n mod prime != 0);
    prime