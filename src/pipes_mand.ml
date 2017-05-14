open Mandelbrot

module PipMand = Mand(Pipes.P)

let _ = Pipes.P.run (PipMand.main 1500 1000 20 2)
