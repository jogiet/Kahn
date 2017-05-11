module M = Marshal
module U = Unix
module B = Bytes
module A = Arg
open Network_utils

(* TODO Dans le cas du client, initialiser 'sock' pour communiquer
   avec le serveur *)
let sock = ref U.stdout

  
module N : Kahn.S =
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

  let buffers = Hashtbl.create 5
    
    
    
    
  let recv_message () =
    let m = recv_obj !sock in
    (* TODO Effectuer les opérations nécessaires lors de la réception du message *)
    match m with
    | Exec p -> Queue.add p micro_threads
    | Message(q,v) ->
       if not (Hashtbl.mem buffers q) then
         Hashtbl.add buffers q (Queue.create ());
      let w = Obj.repr v in
      Queue.add w (Hashtbl.find buffers q)
    | Doco _ | AskChan | AskMess _ ->
       failwith "Client received Doco, AskChan or AskMess"
    | RetChan _ -> failwith "Client received RetChan unexpectedly"
         
       
  let recv_all_messages () =
    while ( U.select [] [!sock] [] 0. ) <> ( [], [], [] ) do
      recv_message ()
    done
      
      
  let ask_for_channel () =
    recv_all_messages ();
    send_obj AskChan !sock;
    let m = recv_obj !sock in
    match m with
    | RetChan q -> q
    | Exec _ | Message _ | Doco _ | AskChan | AskMess _ ->
       failwith "Client received unexpected message while waiting for chan"

  let send_to_server v q =
    send_obj ( Message (q,v) ) !sock

  let send_ask_msg q =
    send_obj ( AskMess q ) !sock

  let received_message q =
    Hashtbl.mem buffers q &&
      let queue = Hashtbl.find buffers q in
      not (Queue.is_empty queue)

  let get_message q =
    let queue = Hashtbl.find buffers q in
    let w = Queue.take queue in
    Obj.obj w

      
  let run_background () =
    recv_all_messages ();
    if not (Queue.is_empty micro_threads) then
      let p = Queue.take micro_threads in
      match p () with
      | Result () -> ()
      | Continue p' ->
         Queue.add p' micro_threads
         
        
  (*---------- Fonctions de l'interface ----------*)
      
  let new_channel () =
    let q = ask_for_channel () in
    q, q

  let put v q () =
    send_to_server v q;
    Result ()

  let get q () =
    (* Lors de get, envoyer la demande au serveur, puis redonner la
       main au client, en executant plus tard en boucle douce (qui à
       chaque tour laisse la main) une attente du message *)
    send_ask_msg q;
    let rec continuation () =
      if received_message q then
        let v = get_message q in
        Result v
      else
        Continue continuation
    in
    Continue continuation

      
  let doco l () =
    
    Result ()

  let return v () =
    Result v

  let rec bind x f () =
    match x () with
    | Result v -> Continue (f v)
    | Continue y -> Continue (bind y f)
       
  let rec run p =
    match p () with
    | Result r -> r
    | Continue p' ->
       run_background ();
       run p'
       
       
end

