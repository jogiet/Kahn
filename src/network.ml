module M = Marshal
module U = Unix
module B = Bytes
module A = Arg
open Network_utils

(* TODO Dans le cas du client, initialiser 'sock' pour communiquer
   avec le serveur *)
let sock = ref U.stdout

module type Extended_kahn =
sig
  include Kahn.S
  val run_worker : unit -> unit
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

  let buffers = Hashtbl.create 5

  let handle_message m =
    (* Effectuer les opérations nécessaires lors de la réception du message *)
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
    send_obj AskChan !sock;
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
    recv_all_messages ();
    if not (Queue.is_empty micro_threads) then
      let p = Queue.take micro_threads in
      match p () with
      | Result () -> ()
      | Continue p' ->
         Queue.add p' micro_threads
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
    send_to_channel v q;
    Result ()

  let get q () =
    (* Lors de get, envoyer la demande au serveur, puis redonner la
       main au client, en exécutant plus tard en boucle "soft" (qui à
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
    send_doco_msg l;
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


let client_main ?task sock' =
  sock := sock';
  match task with
  | None ->  N.run_worker ()
  | Some t -> N.run t

     
     



(*---------- Fonctionnalités serveur ----------*)
     
(* Le serveur accepte un nombre arbitraire de clients.
Dans une boucle infinie, il reçoit les messages, et y répond de façon appropriée. *)
