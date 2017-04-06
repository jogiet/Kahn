(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
(*                      Pipes Kahn Network                      *)
(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

module M = Marshal
module U = Unix
module B = Bytes

module P : Kahn.S = 
struct 
	type 'a process = unit -> 'a
	type 'a channel = Unix.file_descr
	type 'a in_port = 'a channel
	type 'a out_port = 'a channel

	let new_channel () = 
		U.pipe ()
	
	let put x outprt = 
	let res () = 
		let bycode = M.to_bytes x [] in
		ignore (U.write outprt bycode 0 (B.length bycode))
	in res

	let get inprt = 
	let res () =
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
	in res

		
	let doco l =  
	let res () = 
		let rec aux = function
		| [] -> ()
		| t::q -> 
			let i = U.fork () in
			if i = 0 then
			begin
				t ();
				Printf.printf "Ca chiale \n";
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



























