open True_mandelbrot
open Sequential

module SeqMand = Mand(Seq)

let _ = 
	(Seq.run (SeqMand.main ))

