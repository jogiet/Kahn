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
    
  and port = int
  and 'a in_port = port
  and 'a out_port = port

  and 'a message = 
    | Exec of unit process
    | Doco of unit process list
    | AskChan
    | RetChan of int
    | Message of port * 'a
    | AskMess of port



  (*---------- Fonctions utilitaires ----------*)

  let micro_threads = Queue.create ()

  module ProcessQueue = struct
    type t = unit process Queue.t
    let compare a b = a < b
  end
    module QueueMap = 
              
      
      
  let recv_message () =
    let m = recv_obj !sock in
    (* TODO Effectuer les opérations nécessaires lors de la réception du message *)
    match m with
    | Exec p -> Queue.add p micro_threads
    | Message(q,v) -> buffers
       
  let recv_all_messages () =
    while (U.select [] [!sock] [] 0.) <> ([],[],[]) do
      recv_message ()
    done
      
      
  let ask_for_channel () =
    recv_all_messages ();
    send_obj AskChan !sock;
    recv_obj !sock

      


  (*---------- Fonctions de l'interface ----------*)
      
  let new_channel =
    ask_for_channel

  let put v q () =
    (* TODO *)
    Result ()

  let get q () =
    (* TODO Lors de get, envoyer la demande au serveur, puis renvoyer
       Continue d'un nouveau process, qui vérifie si le message est arrivé
       (et s'il ne l'est pas, renvoie Continue de lui-même) *)
    Result (Obj.magic ())
      
  let doco l () =
    (* TODO *)
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
    | Continue p' -> run p'
       
       
end

