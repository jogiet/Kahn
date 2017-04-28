module B = Bytes
module U = Unix
module M = Marshal

let () = 
	let s = U.socket U.PF_INET U.SOCK_STREAM 0 in
	let host_name = "sas" in
	let host = U.gethostbyname host_name in
	let ip_addr = host.h_addr_list.(0) in
	let port = 12345 in
	let addr = U.ADDR_INET (ip_addr,port)
	in begin
		U.connect s addr;
		let bycode = M.to_bytes "Hello World !\n" [] in
			ignore (U.write s bycode 0 (B.length bycode));
		U.shutdown s U.SHUTDOWN_ALL;
	end














