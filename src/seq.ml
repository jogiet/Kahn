(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
(*                     Sequentiel Kahn network                  *)
(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

type 'a process = ('a -> unit) -> unit

type 'a port = 'a Queue.t
type 'a in_port = 'a port
type 'a out_port = 'a port

let new_channel () = 
	let chan_in = Queue.create ()
	and chan_out = Queue.create () in
	(chan_in,chan_out)

let put x out_prt = 
	let res () = 
		() -> 
	





















































