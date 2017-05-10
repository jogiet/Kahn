module M = Marshal
module U = Unix
module B = Bytes
module A = Arg



module N : Kahn.S =
struct 
	type 'a process = unit -> 'a
	and port = int
	and 'a in_port = port
	and 'a out_port = port

	and 'a message = 
		| Exec of unit process
		| Doco of unit process list
		| AskChan
		| RetChan of int
		| Message of port*'a
		| AskMess of port

	
	let new_channel =


end

