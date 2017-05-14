
module G = Graphics
module C = Complex



let rec iter z c n =
	match n with
	| 0 -> z
	| _ -> 
		let zp = iter z c (n-1) in
			C.add (C.mul zp zp) c



let coord_to_compl x y = 
	{C.re = float_of_int (x-800)/.400.;
	 C.im = float_of_int (y-400)/.400.}

let _  = 
	G.open_graph "";
	G.resize_window 1200 800;
	for x = 0 to 1299 do
		for y = 0 to 799 do
			let c = coord_to_compl x y in
			let z = ref (iter c c 10)
			and i = ref 1
			in
			while C.norm !z < 2. && !i < 10 do
				z := iter !z c 10;
				incr i;
			done;
			if !i = 10 then
				G.set_color G.black
			else
				G.set_color (G.rgb  (127+ !i*128/10) 0 0);
			G.plot x y;
		done;
	done;
	G.read_key () |> ignore;
	G.close_graph ();
