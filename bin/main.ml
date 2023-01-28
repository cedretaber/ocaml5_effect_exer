(* open Effect_exer *)

(* exception TestError of string *)

let _ =
  let open Effect in
  (* let open Deep in *)
  let module M = struct
    type _ Effect.t += E : unit Effect.t
  end in
  let open M in
  (* let run f arg =
    try_with f arg {
      effc= fun (type a) (eff : a Effect.t) ->
        match eff with
        | E -> Some (fun (_ : (a, _) continuation) -> 99)
        | _ -> None
    }
  in *)
  let main () =
    perform E;
    print_endline "OK?";
    42
  in
  print_int @@ main ()
