open Primes

module NetPrimes = KahnPrimes(Network_inter.N)

let () = Network_inter.N.run NetPrimes.main

