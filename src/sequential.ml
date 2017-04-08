
open Kahn

module Seq: S = struct

    type 'a result =  Result of 'a | Continue of 'a process
    and 'a process = unit -> 'a result

    type 'a channel = 'a Queue.t
    type 'a in_port = 'a channel
    type 'a out_port = 'a channel
      
    let new_channel () =
      let q = Queue.create () in
      q, q

    let put v q () =
      Result (Queue.add v q)

    let rec get q () =
      try
        let v = Queue.take q in
        Result v
      with Queue.Empty ->
        Continue (get q)

    let rec doco pp () =
      let one_step living_threads proc =
        match proc () with
        | Result () -> living_threads
        | Continue proc' -> (proc' :: living_threads)
      in
      match pp with
      | [] ->
         Result ()
      | _ ->
         Continue (doco (List.fold_left one_step [] pp))

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

open Primes
module SeqPrimes = KahnPrimes(Seq)
let _ = Seq.run SeqPrimes.main

