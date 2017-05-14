open Sequential
open Primes
module SeqPrimes = KahnPrimes(Seq)
let _ = Seq.run (SeqPrimes.main ())

