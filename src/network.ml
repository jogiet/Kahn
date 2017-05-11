module M = Marshal
module U = Unix
module B = Bytes
module A = Arg
open Network_utils

let print s = print_string s; flush_all ()

(* Dans le cas du client, initialiser 'sock' pour communiquer
   avec le serveur *)
let sock = ref U.stdout

module type Extended_kahn =
sig
  include Kahn.S
  val client_main : ?task:(unit -> 'a process) -> U.file_descr -> 'a
  val server_main : U.sockaddr -> unit
end
  
module N : Extended_kahn =
struct

  (*---------- Définition des types ----------*)
  type 'a result =
    | Result of 'a
    | Continue of 'a process
  and 'a process = unit -> 'a result
    
  and 'a port = int
  and 'a in_port = 'a port
  and 'a out_port = 'a port

  and 'a message = 
    | Exec of unit process
    | Doco of unit process list
    | AskChan
    | RetChan of 'a port
    | Message of 'a port * 'a
    | AskMess of 'a port



  (*---------- Fonctions utilitaires ----------*)

  let micro_threads = Queue.create ()
  let next_mthread_id = ref 0

  let buffers = Hashtbl.create 5


  let string_of_message = function
    | Exec _ -> "Exec"
    | Message _ -> "Message"
    | Doco _ -> "Doco"
    | AskChan -> "AskChan"
    | AskMess _ -> "AskMess"
    | RetChan _ -> "RetChan"
    
    
  let handle_message m =
    print ("Handling message " ^ string_of_message m ^ "\n");
    (* Effectuer les opérations nécessaires lors de la réception du
       message *)
    match m with
    | Exec p ->
       let id = !next_mthread_id in
       next_mthread_id := !next_mthread_id +1;
       Queue.add (id,p) micro_threads
       
    | Message(q,v) ->
       if not (Hashtbl.mem buffers q) then
         Hashtbl.add buffers q (Queue.create ());
      let w = Obj.repr v in
      Queue.add w (Hashtbl.find buffers q)
        
    | Doco _ | AskChan | AskMess _ ->
       failwith "Client received Doco, AskChan or AskMess"  
    | RetChan _ ->
       failwith "Client received RetChan unexpectedly"

  let recv_any_message () =
    let m = recv_obj !sock in
    handle_message m
      
  let recv_all_messages () =
    while ( U.select [] [!sock] [] 0. ) <> ( [], [], [] ) do
      recv_any_message ()
    done

  let recv_retchan_message () =
    let q = ref None in
    while !q = None do
      let m = recv_obj !sock in
      match m with
      | RetChan q1 -> q := Some q1
      | _ -> handle_message m
    done;
    match !q with
    | None -> assert false
    | Some q' -> q'
       
  let ask_for_channel () =
    print "Asking for channel\n";
    send_obj AskChan !sock;
    print "Waiting for answer\n";
    recv_retchan_message ()

  let send_to_channel v q =
    send_obj ( Message (q,v) ) !sock

  let send_ask_msg q =
    send_obj ( AskMess q ) !sock

  let send_doco_msg tasks =
    send_obj ( Doco tasks ) !sock

  let received_message q =
    Hashtbl.mem buffers q &&
      let queue = Hashtbl.find buffers q in
      not (Queue.is_empty queue)

  let get_message q =
    let queue = Hashtbl.find buffers q in
    let w = Queue.take queue in
    Obj.obj w

  let wait_a_while () =
    ignore (U.select [] [] [] 0.001)
      
  let run_background () =
    print "Running bg tasks\n";
    recv_all_messages ();
    if not (Queue.is_empty micro_threads) then
      let id,p = Queue.take micro_threads in
      print ("Running micro-process #"^string_of_int id^"\n");
      match p () with
      | Result () -> ()
      | Continue p' ->
         Queue.add (id,p') micro_threads
    else
      wait_a_while ()
        
  let run_worker () =
    while (* TODO handle end of connection ? *) true do
      run_background ()
    done
      
  (*---------- Fonctions de l'interface ----------*)
      
  let new_channel () =
    let q = ask_for_channel () in
    q, q

  let put v q () =
    print ("Put to channel "^string_of_int q);
    send_to_channel v q;
    Result ()

  let get q () =
    (* Lors de get, envoyer la demande au serveur, puis redonner la
       main au client, en exécutant plus tard en boucle "soft" (qui à
       chaque tour laisse la main) une attente du message *)
    print ("Get from channel "^string_of_int q^"\n");
    send_ask_msg q;
    let rec continuation () =
      print ("Waiting to get from channel "^string_of_int q^"\n");
      if received_message q then
        let v = get_message q in
        Result v
      else
        Continue continuation
    in
    Continue continuation

      
  let doco l () =
    match l with
    | [] -> Result ()
    | t1 :: rest ->
       send_doco_msg rest;
      Continue t1

  let return v () =
    Result v

  let rec bind x f () =
    match x () with
    | Result v -> Continue (f v)
    | Continue y -> Continue (bind y f)
       
  let rec run p =
    print "Running main task\n";
    match p () with
    | Result r -> r
    | Continue p' ->
       run_background ();
      run p'
        




  (*---------- Fonctionnalités serveur ----------*)
        
  (* Le serveur accepte un nombre arbitraire de clients.  Dans une
     boucle infinie, il reçoit les messages, et y répond de façon
     appropriée ; il vérifie notamment s'il peut répondre aux 'get' en
     attente *)

  let srv_buffers = Hashtbl.create 5
  let srv_next_buffer = ref 0
  let srv_clients = Queue.create ()
  let srv_client_socks = ref []

  (* For each channel, remember who is waiting for the answer *)
  let srv_waiting = Hashtbl.create 5

  let srv_forward_message client q =
    (* Try to forward the message from channel q to client. Return
       false in case of failure (empty queue). *)
    let queue = Hashtbl.find srv_buffers q in
    if Queue.is_empty queue then
      false
    else
      let w = Queue.take queue in
      let v = Obj.obj w in
      send_obj (Message (q,v)) client;
      true

  let srv_update_channel q =
    if Hashtbl.mem srv_waiting q then
      let client = Hashtbl.find srv_waiting q in
      if srv_forward_message client q then
        Hashtbl.remove srv_waiting q
          
  let srv_handle_message client = function
    | Message (v,q) ->
       let queue = Hashtbl.find srv_buffers q in
       let w = Obj.repr v in
       Queue.add w queue;
       srv_update_channel q
         
    | Doco tasks -> (* TODO Improve task distribution *)
       let send_one_task task =
         let client = Queue.take srv_clients in
         send_obj (Exec task) client;
         Queue.add client srv_clients
       in
       List.iter send_one_task tasks
         
    | AskChan ->
       (* Crée un nouveau channel et le stocke dans srv_buffers, puis
          répond à la question *)
       let q = !srv_next_buffer in
       srv_next_buffer := !srv_next_buffer + 1;
       Hashtbl.add srv_buffers q (Queue.create ());
       send_obj (RetChan q) client
       
    | AskMess q ->
       Hashtbl.add srv_waiting q client;
      srv_update_channel q
        
    | Exec _ | RetChan _ -> failwith "Server received Exec or RetChan"


  let server_main addr =
    let domain = U.domain_of_sockaddr addr in
    let sock = U.socket domain U.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    print "Binding socket, with SO_REUSEADDR\n";
    let _ = U.bind sock addr in
    let _ = U.listen sock 20 in
    while true do
      if (U.select [sock] [] [] 0.) <> ([], [], []) then begin
        print "\nClient connected!\n";
        let client_sock, client_addr = U.accept sock in
        Queue.add client_sock srv_clients;
        srv_client_socks := client_sock :: !srv_client_socks;
      end;
      let received, _, _ = U.select !srv_client_socks [] [] 0.01 in
      let receive_message client =
        let msg = recv_obj client in
        print ("Handling message "^ string_of_message msg ^"\n");
        srv_handle_message client msg
      in
      List.iter receive_message received;
    done
        
  let client_main ?task sock' =
    sock := sock';
    match task with
    | None ->
       print "Launching worker\n";
      run_worker (); assert false
    | Some t ->
       print "Resolving main function...\n";
      let t1 = t () in
      print "Launching main client\n";
      flush_all ();
      run (t1)
        
end




     
     

