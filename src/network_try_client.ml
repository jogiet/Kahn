module B = Bytes
module U = Unix
module M = Marshal

let () = 
  let s = U.socket U.PF_INET U.SOCK_STREAM 0 in
  let host_name = "Tabac" in
  let host = U.gethostbyname host_name in
  let ip_addr = host.U.h_addr_list.(0) in
  let port = 12345 in
  let addr = U.ADDR_INET (ip_addr,port)
  in begin
    print_string "C:Waiting a second to ensure server is alive...\n";
    flush_all ();
    U.sleep 1;
    U.connect s addr;
    print_string "C:Connected. Sending bytes...\n";
    let bycode = M.to_bytes "Hello, you are my favorite server!\n" [] in
    ignore (U.write s bycode 0 (B.length bycode));
    print_string "C:Success!\n";
    U.shutdown s U.SHUTDOWN_ALL;
  end














