module type Aio = sig
  val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
  val recv : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int
  val send : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int
  val fork : (unit -> unit) -> unit
  val run : (unit -> unit) -> unit
  val non_blocking_mode : bool
end

module Default : Aio = struct
  let accept fd = Unix.accept fd
  let recv = Unix.recv
  let send = Unix.send
  let fork f = f ()
  let run f = f ()
  let non_blocking_mode = false
end