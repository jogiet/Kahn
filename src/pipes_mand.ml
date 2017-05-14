open Mandelbrot

module PipMand = Mand(Pipes.P)

let _ = Pipes.P.run (PipMand.main 150 100 20 2)
