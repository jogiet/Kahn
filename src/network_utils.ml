module U = Unix
module M = Marshal
module B = Bytes

let run_client f addr =
  let domain = U.domain_of_sockaddr addr in
  let sock = U.socket domain U.SOCK_STREAM 0 in
  let _ = U.connect sock addr in
  f sock

let run_server service addr =
  let domain = Unix.domain_of_sockaddr addr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  let _ = Unix.bind sock addr in
  let _ = Unix.listen sock 20 in
  while true do
    let client_sock, client_addr = Unix.accept sock in
    ignore (service client_sock client_addr)
  done;
  ()


    
let send_bytes b sock =
  let l = ref (Bytes.length b) in
  while !l > 0 do
    l := !l - (Unix.send sock b 0 !l []);
  done;
  ()

let recv_bytes length sock =
  let l = ref length in
  let b = Bytes.create length in
  while !l > 0 do
    l := !l - (Unix.recv sock b 0 !l []);
  done;
  b


    
let send_obj obj sock =
  let b = M.to_bytes obj [M.Closures] in
  send_bytes b sock

let recv_obj sock =
  let header = recv_bytes M.header_size sock in
  let size = M.data_size header 0 in
  let data = recv_bytes size sock in
  M.from_bytes (B.cat header data) 0
