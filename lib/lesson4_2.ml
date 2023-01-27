
type ('elt, 'container) iterator = ('elt -> unit) -> 'container -> unit
type 'elt generator = unit -> 'elt option