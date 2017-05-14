open Mandelbrotnaif
open Network_inter

module NetInterMand = Mand(N)

let _ = 
	N.run (NetInterMand.main 150 100 5 20)
