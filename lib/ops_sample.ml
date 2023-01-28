let ( >>= ) = Option.bind
let return x = Some x

let sum_opt (x : int option) (y : int option) =
  x >>= fun a ->
  y >>= fun b ->
  return (a + b)

let ( let* ) = Result.bind
let return x = Ok x

let sum_res (x : (int, string) result) (y : (int, string) result) =
  let* a = x in
  let* b = y in
  return (a + b)
