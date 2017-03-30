(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
(*                      Pipes Kahn Network                      *)
(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

module I : Kahn.S = 
struct 
	type 'a process = unit -> 'a
	type 'a in_port = Unix.file_descr
	type 'a out_port = Unix.file_descr

	let new_channel () = 
		Unix.pipe ()
	
	let put x outprt = 
	let res = 
		let bycode = Marshal.to_bytes x [] in
			Unix.write outprt bycode 0 (Bytes.length bycode)
	in res


	let get inprt = 
	let res () = 
		


end



























