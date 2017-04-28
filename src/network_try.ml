module U = Unix
module M = Marshal
module B = Bytes

(* fichier serveur *)


let () =
	let s = U.socket U.PF_INET U.SOCK_STREAM 0 in
	let ip_addr = U.inet_addr_any
	and port = 12345 in
	let addr = U.ADDR_INET (ip_addr,port)
	in begin
		U.bind s addr;
		U.listen s 20;
		let s_cl , addr_cl = U.accept s in
		let header = B.create M.header_size 
		in begin
			ignore (U.read s_cl header 0 M.header_size);
			let size = M.data_size header 0 in
			let data = B.create size 
			in begin
				ignore (U.read s_cl data 0 size);
				let res =  M.from_bytes (B.cat header data) 0
				in print_string res;
				U.shutdown s_cl U.SHUTDOWN_ALL;
			end;
		end;
	end;








