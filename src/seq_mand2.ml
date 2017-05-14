open Mandelbrotnaif
open Network

module SeqMand = Mand(N)

let _ = 
	(N.run (SeqMand.main 60 40 20 5 ))

