(* 1D convolution, this is necessary because many common 2D filters could be*)
(* separated as two 1D filters, and 1D convolution requires far less calculation*)
(* than the 2D convolution does *)
let conv1D x h = 
	let elem n =
		let rec calcElem k y= 
			if (k <= min (-1+Array.length x) n) 
				then calcElem (k+1) y+(Array.get x k)*(Array.get h (n-k))
			else y
			in calcElem (max 0 (n+1-(Array.length h))) 0
	in
	let a=Array.init ((Array.length x)+(Array.length h)-1) elem
in a;;


(* 2D convolution *)
(* Based on my understanding of 2D convolution, and practical usage into consideration*)
(* I think our 2D convolution should has a output with a size exact same to the input*)
(* But this could be discussed and modified if necessary *)

let conv2D xm hm = 
	let calcElem k l=
		for r1=(max 0 1+k-(Array.length hm)) to (min k (-1+Array.length xm)) do
			for c1=(max 0 1+l-(Array.length hm.(0))) to (min l (-1+Array.length xm.(0))) do
				elem=xm.(r1).(c1)*hm.(k-r1).(l-c1)
			done
		done;
		elem (* an error says that elem is an unbound value, have no idea about why :( *)
	in
	
	let a=Array.make_matrix (Array.length inm) (Array.length inm.(0)) 0 in
	let rec outM= 
		for r=0 to (Array.length a)-1 do 
			for c=0 to (Array.length a.(0))-1 do
				 a.(r).(c) <- (calcElem r c);
			done
		done;
		a
in outM