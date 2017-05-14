module M = Marshal
module U = Unix
module B = Bytes
module NU = Network_utils

module N : Kahn.S =
struct 
	type 'a process = unit -> 'a
	type 'a channel = U.file_descr
	type 'a in_port = 'a channel
	type 'a out_port = 'a channel

	let port = ref 12345

	let get_port () = 
	begin
		incr port;
		!port;
	end

	let new_channel () = 
		let addr = U.ADDR_INET (U.inet_addr_loopback, get_port ()) in
		let domain = U.domain_of_sockaddr addr in
		let sock_in = U.socket domain U.SOCK_STREAM 0 
		and sock_out = U.socket domain U.SOCK_STREAM 0 
		in begin
			U.setsockopt sock_out U.SO_REUSEADDR true;
			U.bind sock_out addr;
			U.listen sock_out 1;
			U.connect sock_in addr;
			let (sock_out, _) = U.accept sock_out in
				(sock_in,sock_out)
		end

	let put x outprt = 
	let res () = 
		NU.send_obj x outprt
		(*
		let bycode = M.to_bytes x [] in
		ignore (U.write outprt bycode 0 (B.length bycode))
		*)
	in res

	let get inprt = 
	let res () =
		NU.recv_obj inprt
		(*
		let header = B.create M.header_size in
		(* D'abord on extrait le header pour avoir la taille des données *)
		begin
			ignore(U.read inprt header 0 M.header_size);
			let size = M.data_size header 0 in
			(* Puis on extrit les données *)
			let data = B.create size 
			in begin
				ignore (U.read inprt data 0 size);
				M.from_bytes (B.cat header data) 0; 
			end;
		end
		*)
	in res

	let doco l =  
	let res () = 
		let rec aux = function
		| [] -> ()
		| [x] -> x ()
		| t::q -> 
			let i = U.fork () in
			if i = 0 then
			begin
				t ();
				exit 0;
			end
			else
			begin	
				aux q;
				ignore(U.wait ());
			end
		in aux l
	in res

	let return x = (fun () -> x)

	let bind e e' = 
		fun () -> e' (e ()) ()


	let run e = e ()

end















































