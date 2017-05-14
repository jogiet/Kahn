open True_mandelbrot
open Sequential

module SeqMand = Mand(Seq)

let _ = 
	Printf.printf "Un test comme un autre \n";
	(Seq.run (SeqMand.main ))

