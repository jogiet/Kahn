open Kahn
module G = Graphics
module C = Complex
module S = String
module A = Array


module Mand (K : Kahn.S) = struct
	module K = K
	module KLib = Lib(K)
	open KLib

	type elet = 
		{x : int; 
		 y : int; 
		 c : C.t;
		 mutable z : C.t;
		 mutable n : int } (*nombre d'itérations effectuées *)
	

	type sent_iter = 
		| End_i
		| ToDo_i of elet
	
	type pix = 
		{x : int;
		 y : int;
		 mutable c : G.color}

	type sent_color = 
		| End_c
		| ToDo_c of pix

	let input 
		(x_size : int)
		(y_size : int)
		(q_out : sent_iter K.out_port)
		: unit K.process = 
		let rec loop x y = 
		if y = y_size then
			K.put End_i q_out
		else if x = x_size then
			loop 0 (y+1)
		else
		begin
			let x_size_f = float_of_int x_size
			and y_size_f = float_of_int y_size in
			let z = 
				{C.re = 3.*.(float_of_int x)/.x_size_f -. 2.;
				 C.im = 2.*.(float_of_int y)/.y_size_f -. 1.} in
			let to_send = {x = x ; y = y ; c = z ; z = z ; n = 0} in
				K.put (ToDo_i to_send) q_out >>=
				(fun () -> loop (x+1) y)
		end
		in loop 0 0


	let iter 
			(q_in : sent_iter K.in_port) 
			(q_next : sent_iter K.out_port) 
			(q_out : sent_iter K.out_port) 
		: unit K.process = 
		let rec loop () = 
			(K.get q_in) >>= 
			(fun sent -> 
				let rec iter z c = function 
				| 0 -> z
				| n -> let res = iter z c (n-1) in
					C.add (C.mul res res) c

			in match sent with
			| End_i ->
			begin
				K.doco [K.put End_i q_next; K.put End_i q_out]
			end
			| ToDo_i pix ->
			begin
				pix.z <- iter pix.z pix.c 10; 
				(* par défaut on itère 10 fois pour un processus *)
				(if C.norm pix.z < 2. then
				begin
					pix.n <- pix.n + 1;
					K.put (ToDo_i pix) q_next 
				end
				else
					K.put (ToDo_i pix) q_out) >>=
				(fun () ->loop ())
			end)
	in
	loop ()
	
	let color 
			(n : int) (* Nombre d'itérations restantes *)
			(q_in : sent_iter K.in_port)
			(q_out : sent_color K.out_port)
		: unit K.process = 
		let rec loop n_tot n =
			(K.get q_in) >>=
			(fun elet -> match elet with
			 | End_i -> 
			 	if n = 0 then
					K.put End_c  q_out 
				else
					loop n_tot (n-1)
			| ToDo_i elet -> 
				let to_send = 
					{x = elet.x;
					 y = elet.y;
					 c = if elet.n = n_tot then
					 		G.black
						 else
						 	G.rgb 0 (128 +127*(elet.n+1)/n_tot) 0} 
				in	K.put (ToDo_c to_send) q_out >>=
				(fun () -> loop n_tot n)) 
		in loop n n 

	let assemble	
		(x_size : int)
		(y_size : int)
		(q_in : sent_color K.in_port)
		(q_out : G.color array array K.out_port)
		: unit K.process = 
		let rec loop tab =
			K.get q_in >>=
			(fun pix -> match pix with
			| End_c -> 
				K.put  tab q_out 
			| ToDo_c pix ->
			begin
				tab.(pix.x).(pix.y) <- pix.c;
				loop tab;
			end )
		in loop (Array.make_matrix x_size y_size G.blue)


	let print
		(x_size : int)
		(y_size : int)
		(q_in : G.color array array K.in_port)
		(q_out : string K.out_port)
		: unit K.process =
		(K.get q_in) >>= 
		(fun tab ->
			G.open_graph "";
			G.resize_window x_size y_size;
			for x= 0 to x_size -1 do
				for y = 0 to y_size -1 do
					G.set_color tab.(x).(y);
					G.plot x y;
				done
			done;
			G.read_key () |> ignore;
			K.put "On a fini : appuyer sur une touche pour quitter \n"
			q_out ;
			)


	let main : unit K.process =
	Printf.printf "On est près à lancer les processus \n";
	let x_size = 60 in
	let y_size = 40 in
	let n_tot = 2 in
	let chan = A.map (K.new_channel) (Array.make (n_tot+1) ()) in  
	let a_chan = K.new_channel () in
	let p_chan = K.new_channel () in
	let f_chan = K.new_channel () in
	Printf.printf "On est près à lancer les processus \n";
	let process_l = ([input x_size y_size (snd chan.(0))]@
		(A.to_list 
			(A.mapi 
				(fun i ch -> iter (fst ch) (snd chan.(i+1))
				 (snd chan.(n_tot -1)))
				(A.sub chan 0 (n_tot -1))
			))@
			[color n_tot (fst chan.(n_tot -1)) (snd a_chan)]@
			[assemble x_size y_size (fst a_chan) (snd p_chan)]@
			[print x_size y_size (fst p_chan) (snd f_chan)]) 
	in begin 
	Printf.printf "On est près à lancer les processus \n";
	K.doco process_l;
	end
		

end
