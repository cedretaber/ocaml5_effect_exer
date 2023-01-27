module type EQUAL = sig
  type t
  val equals : t -> t -> bool
end

type 'a tree =
  | Node of 'a tree * 'a tree
  | Leaf of 'a

module SameFringe (E : EQUAL) : sig
  val call : E.t tree -> E.t tree -> bool
end = struct
  open Effect
  open Deep

  type _ Effect.t += Yield : E.t -> unit Effect.t

  let rec walk = function
    | Node (left, right) -> walk left; walk right
    | Leaf e -> perform (Yield e)

  type step =
    | Done
    | Yielded of E.t * (unit, step) continuation

  let step f =
    match_with f () {
      retc= (fun _ -> Done);
      exnc= (fun e -> raise e);
      effc= fun (type c) (eff : c Effect.t) ->
        match eff with
        | Yield e -> Some (fun (k : (c, _) continuation) -> Yielded (e, k))
        | _ -> None
    }

  let rec run s t =
    match s (), t () with
    | Done, Done -> true
    | Yielded (e1, k1), Yielded (e2, k2) when E.equals e1 e2 -> run (fun () -> continue k1 ()) (fun () -> continue k2 ())
    | _ -> false

  let call t1 t2 =
    run
      (fun () -> step (fun () -> walk t1))
      (fun () -> step (fun () -> walk t2))
end

module SameFringe_Int = SameFringe(struct
  type t = int
  let equals a b = a = b
end)

let t1 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
let t2 = Node (Node (Leaf 1, Leaf 2), Leaf 3)
let t3 = Node (Node (Leaf 3, Leaf 2), Leaf 1)
let t4 = Leaf 42
let t5 = Leaf 41
let t6 = Node (Leaf 1, Leaf 2)
let t7 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
;;

let main () =
  Printf.printf "%b\n" @@ SameFringe_Int.call t1 t2;
  Printf.printf "%b\n" @@ SameFringe_Int.call t2 t1;
  Printf.printf "%b\n" @@ SameFringe_Int.call t1 t3;
  Printf.printf "%b\n" @@ SameFringe_Int.call t1 t7;
  Printf.printf "%b\n" @@ SameFringe_Int.call t2 t7