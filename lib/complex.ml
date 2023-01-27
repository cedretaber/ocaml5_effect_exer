module StringResult = Result_effect.Make(struct type e = string end)

module StringId = Idgen_effect.MakePerformer(struct type t = string end)

module StringIdRunner = Idgen_effect.Make(struct
  type t = string
  type state = int
  let init () = 0
  let gen state = (string_of_int state, state + 1)
end)(StringId)

let main () =
  let main () =
    let id1 = StringId.gen () in
    let id2 = StringId.gen () in
    let n1 = StringResult.from (Ok 42) in
    let n2 = StringResult.from (Ok 99) in
    Printf.printf "%s: %d ... %s: %d\n" id1 n1 id2 n2;
    let id3 = StringId.gen () in
    let n3 = StringResult.from (Error "Expected error.") in
    let id4 = StringId.gen () in
    Printf.printf "%s %d %s\n" id3 n3 id4;
    print_endline "Unexpected reach."
  in
  match StringResult.run (fun () -> StringIdRunner.run main ()) () with
  | Ok _ -> print_endline "Unexpected reach."
  | Error msg -> print_endline msg