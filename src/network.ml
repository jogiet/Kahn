module M = Marshal
module U = Unix
module B = Bytes
module A = Arg
open Network_utils

let verbose = ref false
let print s = if !verbose then begin print_string s; flush_all (); end; ()

let print_hashtbl tbl =
  print "Hashtbl : [";
  let f k v = print (string_of_int k ^ ";") in
  Hashtbl.iter f tbl;
  print "]\n"


let buffers : (int, Obj.t Queue.t) Hashtbl.t  = Hashtbl.create 5

  
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

  type t = Obj.t
  
  type port = int
  and 'a in_port = port
  and 'a out_port = port
    
  type result =
    | Result of t
    | Continue of my_process
  and process_to_run = unit -> result
  and my_process =
    | Put of t * port
    | Get of port
    | Bind of my_process * (t -> my_process)
    | Run of process_to_run
        
  type 'a process = my_process
    
  and 'a message = 
    | Exec of unit process
    | Doco of unit process list
    | AskChan
    | RetChan of port
    | Message of port * 'a
    | AskMess of port



  (*---------- Fonctions utilitaires ----------*)

  let micro_threads = Queue.create ()
  let next_mthread_id = ref 0

  let _ = print_hashtbl buffers

  let failure_counter = ref 0
  let failure () =
    failure_counter := !failure_counter + 1;
    if (*!failure_counter > 10*) false then begin
      print "***Too many failures. Exiting.***\n";
      exit 1
    end


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
       print ("Receiving msg on channel @"^string_of_int q^"\n");
      if not (Hashtbl.mem buffers q) then begin
        print ("Creating buffer for channel @"^string_of_int q^"\n");
        let queue = Queue.create () in
        Hashtbl.add buffers q queue;
      end;
      assert (Hashtbl.mem buffers q);
      let w = Obj.magic v in
      let queue = Hashtbl.find buffers q in
      Queue.add w queue;
      print_hashtbl buffers
        
    | Doco _ | AskChan | AskMess _ ->
       failwith "Client received Doco, AskChan or AskMess"  
    | RetChan _ ->
       failwith "Client received RetChan unexpectedly"

  let recv_any_message () =
    let m = recv_obj !sock in
    handle_message m
      
  let recv_all_messages () =
    while ( U.select [!sock] [] [] 0. ) <> ( [], [], [] ) do
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
    print "Sending DOCO message.\n";
    send_obj ( Doco tasks ) !sock
      
      
  let received_message q =
    (* print ("received_message @"^string_of_int q^" : ");*)
    (* print_hashtbl buffers;*)
    if Hashtbl.mem buffers q then begin
      (* print "Checking buffer\n";*)
      let queue = Hashtbl.find buffers q in
      not (Queue.is_empty queue)
    end else begin
      (* print "No buffer\n"; *)
      false
    end

  let get_message q =
    let queue = Hashtbl.find buffers q in
    let w = Queue.take queue in
    Obj.magic w

  let wait_a_while () =
    ignore (U.select [] [] [] 0.001)


      
  let run_put v q () =
    (* print ("Put to channel @"^string_of_int q^"\n"); *)
    (* print_hashtbl buffers; *)
    send_to_channel v q;
    Result (Obj.magic ())

      
  let run_get q () =
    (* Lors de get, envoyer la demande au serveur, puis redonner la
       main au client, en exécutant plus tard en boucle "soft" (qui à
       chaque tour laisse la main) une attente du message *)
    print ("Get from channel @"^string_of_int q^"\n");
    print_hashtbl buffers;
    send_ask_msg q;
    let rec continuation () =
      (* print ("Waiting to get from channel @"^string_of_int q^"\n"); *)
      (* print_hashtbl buffers; *)
      if received_message q then begin
        print ("Received answer from channel @"^string_of_int q^"\n");
        let v = get_message q in
        Result v
      end else begin
        failure ();
        Continue(Run(continuation))
      end
    in
    Continue(Run(continuation))


  let rec run_bind x (f : t -> my_process) () =
    (* print "run_bind : "; *)
    let x' = run_step x in
    let f' v = f (Obj.magic v) in
    match x' with
    | Result v -> (* print "run_bind|Result\n"; *) Continue (f v)
    | Continue y -> (* print "run_bind|Continue\n"; *) Continue (bind y f')

  and bind x f =
    let f' v =
      f (Obj.magic v)
    in
    Bind(x,f')

      
  and run_step = function
    | Put(v,q) -> (* print "Put -> "; *) run_put v q ()
    | Get(q) -> print "Get -> "; run_get q ()
    | Bind(x,f) ->
       run_bind x f ()
    | Run(f) -> (* print "Run -> "; *) f ()
      
  let run_background () =
    (* print "Running bg tasks\n"; *)
    recv_all_messages ();
    if not (Queue.is_empty micro_threads) then
      let id,p = Queue.take micro_threads in
      (* print ("Running micro-process #"^string_of_int id^"\n"); *)
      (* print_hashtbl buffers; *)
      match run_step p with
      | Result _ -> ()
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

  let put v q =
    let w = Obj.magic v in
    Put(w,q)
      
  let get q =
    Get(q)

  let rec infinite_loop () =
    Continue(Run(infinite_loop))
      
  let run_doco l () =
    match l with
    | [] -> Result (Obj.magic ())
    | rest ->
       send_doco_msg rest;
      infinite_loop ()

  let doco l =
    Run(run_doco l)

  let return v =
    Run( fun () -> Result (Obj.magic v) )
       
  let rec run p =
    (* print "Running main task\n"; *)
    match run_step p with
    | Result r -> Obj.magic r
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
      let v = Obj.magic w in
      send_obj (Message (q,v)) client;
      true

  let srv_update_channel q =
    (* print ("srv_update_channel @"^string_of_int q^" : ");*)
    if Hashtbl.mem srv_waiting q then begin
      let client = Hashtbl.find srv_waiting q in
      if srv_forward_message client q then begin
        print ("Forwarding msg on channel @"^string_of_int q^"\n");
        Hashtbl.remove srv_waiting q
      end else
        print ("Cannot forward to channel @"^string_of_int q^"\n")
    end else
  (* print ("None waiting.\n"); *) ()
          
  let srv_handle_message client = function
    | Message (q,v) ->
      (* print ("Receiving msg on channel @"^string_of_int q^"\n");*)
      let queue = Hashtbl.find srv_buffers q in
      let w = Obj.magic v in
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
        (* print ("Handling message "^ string_of_message msg ^"\n");*)
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




     
     

