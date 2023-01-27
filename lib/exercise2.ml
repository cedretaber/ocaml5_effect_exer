open Printf
open Effect
open Effect.Shallow

module type STATE = sig
  type t
  val get : unit -> t
  val put : t -> unit
  val history : unit -> t list
  val run : (unit -> unit) -> init : t -> unit
end

module State (S : sig type t end) : STATE with type t = S.t = struct
  type t = S.t

  type _ Effect.t += 
    | Get : t Effect.t
    | Put : t -> unit Effect.t
    | History : (t list) Effect.t

  let get () = perform Get

  let put t = perform (Put t)

  let history () = perform History

  let run f ~init =
    let rec loop : type a r. t -> t list -> (a, r) continuation -> a -> r =
      fun state history k x ->
        continue_with k x {
          retc= Fun.id;
          exnc= (fun e -> raise e);
          effc= (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Get -> Some (fun (k : (b, r) continuation) -> loop state history k state)
            | Put t -> Some (fun (k : (b, r) continuation) -> loop t (state :: history) k ())
            | History -> Some (fun (k : (b, r) continuation) -> loop state history k (state :: history))
            | _ -> None)
        }
    in
    loop init [] (fiber f) ()
end

module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

let test () : unit =
  printf "%d\n" (IS.get ());
  printf "%s\n" (SS.get ());
  IS.put 99;
  printf "%d\n" (IS.get ());
  IS.put 100;
  SS.put "fuga";
  printf "%s\n" (SS.get ());
  printf "%s\n" (SS.get ());
  SS.put "piyo";
  printf "%s\n" (IS.history () |> List.map string_of_int |> String.concat ",");
  printf "%s\n" (SS.history () |> String.concat ",")
  
let main () = IS.run ~init:42 (fun () -> SS.run ~init:"hoge" test)