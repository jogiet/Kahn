open Example

module K = Network_inter.N
module E = Example(K)

let () = K.run (E.main ())

