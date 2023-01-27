open Effect
open Deep

module Make(S : sig type e end) = struct
  type _ Effect.t += ResultEffect : ('a, S.e) result -> 'a Effect.t
  
  let from (res : ('a, S.e) result) : 'a = perform (ResultEffect res)
  
  let run f arg : ('a, S.e) result =
    match_with f arg {
      retc= (fun x -> Ok x);
      exnc= (fun e -> raise e);
      effc= fun (type c) (eff : c Effect.t) ->
        match eff with
        | ResultEffect (Ok x) -> Some (fun (k : (c, _) continuation) -> continue k x)
        | ResultEffect (Error e) -> Some (fun _ -> Error e)
        | _ -> None
    }
end

module StringResult = Make(struct type e = string end)

let main () =
  let main () =
    let a = Ok 42 in
    let b = Ok 99 in
    print_int (StringResult.from a + StringResult.from b);
    print_endline "";
    let x = Ok "hoge" in
    let y = Ok "fuga" in
    print_endline (StringResult.from x ^ StringResult.from y);
    let c = Error "Error, but expected!" in
    print_int (StringResult.from a + StringResult.from c);
    print_endline "";
    print_endline "Should not reach here!"
  in
  match StringResult.run main () with
  | Ok _ -> print_endline "Should not reach here!"
  | Error e -> print_endline e;