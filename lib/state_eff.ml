open Effect
open Deep

module Make(S : sig type t end) = struct
  type t = S.t

  type _ Effect.t +=
    | Get : t Effect.t
    | Set : t -> unit Effect.t

  let get () = perform Get
  let set s = perform (Set s)

  let run ~(init : t) f arg =
    let state = ref init in
    match_with f arg {
      retc= (fun res -> state, res);
      exnc= raise;
      effc= fun (type c) (eff : c Effect.t) ->
        match eff with
        | Get -> Some (fun (k : (c, _) continuation) -> continue k !state)
        | Set s -> Some (fun (k : (c, _) continuation) ->
            state := s;
            continue k ()
        )
        | _ -> None
    }
end

module StringState = Make(struct type t = string end)

let main () =
  let main () =
    let s = StringState.get () in
    print_endline s;
    StringState.set "fuga";
    let s = StringState.get () in
    print_endline s;
    let t = StringState.get () in
    print_endline t
  in
  StringState.run ~init:"hoge" main ()
