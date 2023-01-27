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

let main () = ()