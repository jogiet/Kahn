open Network
open Primes
open Printf

module NetPrimes = KahnPrimes(N)

let main =
  let is_server = ref false in
  let server_address = ref "127.0.0.1" in
  let port = ref 12345 in
  let is_worker = ref false in
  let speclist = [
    "-s", Arg.Set is_server, "launch server";
    "-a", Arg.Set_string server_address, "server address";
    "-port", Arg.Set_int port, "port";
    "-w", Arg.Set is_worker, "worker client : waits for instructions"
  ] in
  let usage = "Usage : " in
  Arg.parse speclist (fun _ -> failwith "No anonymous arguments allowed") usage;
  if !is_server then begin
    print_string "Launching server...\n";
    let ip_addr = U.inet_addr_any in
    let addr = U.ADDR_INET (ip_addr, !port) in
    print_string (U.string_of_inet_addr ip_addr);
    print_string "\n";
    flush_all ();
    N.server_main addr
  end else begin
    print_string "Launching client...\n";
    flush_all ();
    let ip_addr = U.inet_addr_of_string !server_address in
    let addr = U.ADDR_INET (ip_addr, !port) in
    if !is_worker then
      let task = None in
      Network_utils.run_client (N.client_main ?task) addr
    else
      let task = NetPrimes.main in
      Network_utils.run_client (N.client_main ~task) addr
  end
    
