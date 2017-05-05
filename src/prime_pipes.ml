open Primes
module PipPrimes = KahnPrimes(Pipes.P)

let () = Pipes.P.run PipPrimes.main

