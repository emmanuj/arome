(*
	1nnr.ml 
	++++++++
	This porgram implements the 1-NNR Algorithm in ocaml using brute force. 
	Attempt have been made to avoid most of the imperative features of the ocaml language

	Copyright(C) 2013. Emmanuel U. John
	This program has been licensed under the GNU public license v2 (GPL V2). You can find 
	the full license in the program archive as 'LICENSE.'

	Author contact:
	emmanuj@clemson.edu
	149 University village dr,
	Central, SC 29630

*)

(* calculates the length of a list*)
let length = function(lst)-> List.length lst;;

(* calculates the square of the difference between
two floating point numbers *)
let square = function(x1,x2)-> (x1 -. x2) *. (x1 -. x2);;

(* sums a list of floating points *)
let rec sumSquares = function(lst)-> 
	if((List.tl lst)==[]) then (List.hd lst)
	else ((List.hd lst ) +. sumSquares((List.tl lst)));;

(* produces the square differences between two lists of floating points*)
let rec squares = function(v1, v2) -> 
	if((List.tl v1)==[] ) then []
	else
		square((List.hd v1),(List.hd v2) )::squares((List.tl v1),(List.tl v2));;		

(* finds the sqrt of the sum of squares *)
let rec distance = function(v, vset, n)->
	if(n == (length(vset))) then []
	else sqrt(sumSquares(squares(v, (List.nth vset n))))::distance(v, vset, n+1);;

(* calculate the distances of the nearest neighbor in vector set vset of vector v*)
let distanceAllVectors2 = function(v, vset)->
	distance(v,vset,0);;

(* taken from Dr Robert Schalk(Clemson University) lecture notes *)
let rec listMin = function(x:float list)->
	if x==[] then
		failwith "listMin should not be used on an empty list"
	else
		if List.tl(x)==[] then List.hd(x)
		else min (List.hd x) (listMin(List.tl x));;

(* returns the index of an element in a list*)
let rec findIdx = function(x,lst, n)->
	if(x==(List.nth lst n)) then n
	else findIdx(x,lst,n+1);;

(* returns a tuple containing the minimum distance and respective 
list index where the minimum occurs *)
let t1NNR = function(distlist)-> 
	(listMin(distlist),(findIdx(listMin(distlist),distlist,0)));;

(* returns the class of the vector which for our purpose is the last element in the vector *)
let vclass = function(v)-> List.nth v (length(v)-1);;

(* returns the the vector in trset at position loc *)
let closest = function((min, loc), trset) -> 
		List.nth trset loc
	;;

(* returns the class of the vector in trset at position loc *)
let closest_class = function((min, loc),trset) -> 
		vclass((List.nth trset loc))
	;;

(* returns the class of the vector in ts closest to x using the 1-NNR algorithm*)
let class_NNR = function(x, ts)->
	closest_class(t1NNR(distanceAllVectors2(x,ts)),ts)
;;