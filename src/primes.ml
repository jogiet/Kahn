open Kahn

module KahnPrimes (K : S) = struct

  module KLib = Lib(K)
  open KLib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "%d@." v; loop ())
    in
    loop ()

  
  let filter prime qi qo =
    let rec loop () =
      (K.get qi) >>=
        (fun n ->
          if n mod prime <> 0
          then
            (K.put n qo) >>= loop
          else
            loop ()
        )
    in
    loop ()

  let rec sift qi qo =
    (K.get qi) >>= (fun prime ->
      (K.put prime qo) >>= (fun () ->
        let qi',qo' = K.new_channel () in
        K.doco [ filter prime qi qo' ; sift qi' qo ]
       )
     )

  let main () =
    let qi1, qo1 = K.new_channel () in
    let qi2, qo2 = K.new_channel () in
    K.doco [ integers qo1 ; sift qi1 qo2 ; output qi2 ]

end
  
