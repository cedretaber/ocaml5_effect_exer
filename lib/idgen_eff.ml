open Effect
open Deep

module type PerformerType = sig
  type t

  type _ Effect.t += IdGen : t Effect.t
  val gen : unit -> t
end

module MakePerformer(S : sig type t end) : PerformerType with type t = S.t = struct
  type t = S.t
  type _ Effect.t += IdGen : t Effect.t
  let gen () = perform IdGen
end

module type This = sig
  type t
  val run : ('a -> 'b) -> 'a -> 'b
end

module Make(S : sig
  type t
  type state
  val init : unit -> state
  val gen : state -> t * state
end)(P : PerformerType with type t = S.t) : This with type t = P.t = struct
  type t = P.t

  let run f arg =
    let state = ref (S.init ()) in
    match_with f arg {
      retc= Fun.id;
      exnc= (fun e -> raise e);
      effc= fun (type c) (eff : c Effect.t) ->
        match eff with
        | P.IdGen -> Some (fun (k : (c, _) continuation) ->
            let id, s = S.gen !state in
            state := s;
            continue k id
          )
        | _ -> None
    }
end

module NumberIdPerformer = MakePerformer(struct type t = int end)

module NumberId = Make(struct
  type t = int
  type state = int
  let init () = 0
  let gen state = (state, state + 1)
end)(NumberIdPerformer)

let main () =
  let main () =
    let i = NumberIdPerformer.gen () in
    Printf.printf "ID: %d \n" i;
    let i = NumberIdPerformer.gen () in
    Printf.printf "ID: %d \n" i;
    let i = NumberIdPerformer.gen () in
    Printf.printf "ID: %d \n" i;
    print_endline "Finish"
  in
  NumberId.run main ()

module StringIdPerformer = MakePerformer(struct type t = string end)

module StringId1 = Make(struct
  type t = string
  type state = int
  let init () = 0
  let gen state = (string_of_int state, state + 1)
end)(StringIdPerformer)

module StringId2 = Make(struct
  type t = string
  type state = unit
  let init () = Random.init 0
  let gen _ = (string_of_int @@ Random.int 100, ())
end)(StringIdPerformer)

let main2 () =
  let main () =
    let i = StringIdPerformer.gen () in
    Printf.printf "ID: %s \n" i;
    let i = StringIdPerformer.gen () in
    Printf.printf "ID: %s \n" i;
    let i = StringIdPerformer.gen () in
    Printf.printf "ID: %s \n" i;
    print_endline "Finish"
  in
  StringId1.run main ();
  StringId2.run main ()
