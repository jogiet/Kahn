open Mandelbrot
open Sequential

module SeqMand = Mand(Seq)

let _ = 
	(Seq.run (SeqMand.main 1500 1000 20 5 ))

