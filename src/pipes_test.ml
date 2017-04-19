open Example

module K = Pipes.P
module E = Example(K)

let () = K.run E.main
