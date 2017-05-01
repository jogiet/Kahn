module U = Unix
module M = Marshal
module B = Bytes

(* fichier serveur *)

let my_print_string s =
  print_string s; flush_all ()

let () =
  let ip_addr = U.inet_addr_any
  and port = 12345 in
  let addr = U.ADDR_INET (ip_addr,port) in
  let domain = Unix.domain_of_sockaddr addr in
  let s = U.socket domain U.SOCK_STREAM 0 in
  U.bind s addr;
  U.listen s 20;
  my_print_string "S:Created. Accepting clients...\n";
  let s_cl , addr_cl = U.accept s in
  my_print_string "S:Client connected.\n";
  let header = B.create M.header_size 
  in
  ignore (U.read s_cl header 0 M.header_size);
  let size = M.data_size header 0 in
  let data = B.create size 
  in
  my_print_string "S:Reading data.\n";
  ignore (U.read s_cl data 0 size);
  let res =  M.from_bytes (B.cat header data) 0
  in
  print_string "S:The message is : ";
  print_string res;
  my_print_string "S:How nice. Exiting.\n";
  U.shutdown s_cl U.SHUTDOWN_ALL;








