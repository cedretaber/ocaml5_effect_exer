
type _ Effect.t += Exception : string -> unit Effect.t

let try_ f args =
  let open Effect.Deep in
  try_with f args { effc= fun (type c) (eff : c Effect.t) ->
    match eff with
    | Exception err -> Some (fun (_ : (c, _) continuation) ->
      Printf.fprintf stderr "Failed. %s%!" err
    )
    | _ -> None
  }

let main () =
  let f1 () =
    print_endline "Success!"
  in
  let f2 () =
    Effect.perform (Exception "Failed!");
    print_endline "Never reach here."
  in
  try_ f1 ();
  try_ f2 ()