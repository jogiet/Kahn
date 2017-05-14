open Mandelbrot
open Network_inter

module NetInterMand = Mand(N)

let _ = 
	N.run (NetInterMand.main 1200 1000 5 20)
