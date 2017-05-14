open True_mandelbrot

module PipMand = Mand(Pipes.P)

let _ = Pipes.P.run (PipMand.main)
