Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* QUESTION 1 *)
(* Write a function foo whose type is (’a -> ’b) -> (’b list -> ’c) -> ’a -> ’c list *)

fun foo f g x = [g [f x]]



(* QUESTION 2 *)
(* Write a function bar x, where x is an integer, that returns a function that takes a list L of integers and returns a list each of whose elements result from multiplying the corresponding element of L by x. Do not use the built-in map function. Rather, write any recursion that is needed yourself. You should not define any function outside of bar *)

fun bar x [] = []
| bar x (l::ls) = x * l :: bar x ls



(* QUESTION 3 *)
(* In preparation for writing a partition sort function (below), define a function part that takes an integer x and an integer list L as parameters and partitions L based on x. That is, part returns a tuple of two lists, such that the first list contains all elements of L less than x and the second list contains the elements of L that are greater than or equal to x *)

fun part x [] = ([], [])
| part x (l::ls) = 
	let
		val (small, big) = part x ls
	in
		if l < x then
			(l::small, big)
		else
			(small, l::big)
	end

(* part 6 [5,2,8,4,1,9,6,10]; *)



(* QUESTION 4 *)
(* Implement a partition sorting function, partSort which uses your part function, above, to sort a list L of integers, returning a sorted list of the elements of L. A partition sort is essentially Quicksort, but without the property of using only a constant amount of space. The algorithm, given a list L, is:
• If L is empty or contains a single element, return L (use pattern-matching for these two cases, not a conditional).
• Otherwise, assuming L is of the form (x::xs), partition xs into two lists based on x by calling your part function.
• Recursively call partSort on each of the two lists, to sort each of the lists.
• Return a single sorted list containing the elements from the first sorted list, followed by
x, followed by the elements of the second sorted list. *)

fun partSort [] = []
| partSort [x] = [x]
| partSort (x::xs) = 
	let
		val (small,big) = part x xs
	in
		partSort small @ [x] @ partSort big
	end

(* partSort [5,2,9,10,12,4,8,1,19]; *)



(* QUESTION 5 *)
(* Implement a polymorphic partition sort function, pSort, that will sort a list of elements of any type. In order to do this, though, pSort must take an additional parameter, the < (less-than) operator, that operates on the element type of the list. *)

fun pSort (op <) [] = []
| pSort (op <) [x] = [x]
| pSort (op <) (x::xs) = 
	let
		fun ps (op <) [] = ([],[])
		| ps (op <) (l::ls) = 
			let 
				val (a,z) = ps (op <) ls
			in
				if l < x then
					(l::a,z)
				else
					(a,l::z)
			end
		val (a,z) = ps (op <) xs
	in
		pSort (op <) a @ [x] @ pSort (op <) z
	end

(* pSort (op <) [1, 9, 3, 6, 7]; *)
(* pSort (fn(a,b) => length a < length b) [[1, 9, 3, 6], [1], [2,4,6], [5,5]]; *)



(* QUESTION 6 *)
(* Write a function reduce that takes a function f and a list L, such that if L is of the form [x1, x2, ..., xn−1, xn], then reduce returns the result of f x1 (f x2 ...(f xn−1 xn)...) *)

exception reduce_error;

fun reduce f [] = raise reduce_error
| reduce f [x] = x
| reduce f (x::xs) = f x (reduce f xs)

(* fun g x y = x + y; reduce g [1,2,3,4,5]; *)
(* reduce (fn x => fn y => x + y) [1,2,3,4,5]; *)



(* QUESTION 7 *)
(* Define a polymorphic tree datatype (i.e. datatype ’a tree = ...) such that a leaf is labeled with an ’a and an interior node has a list of children, each of type ’a tree. That is, each interior node can have an arbitrary number of children, rather than just two (as in a binary tree). *)

datatype 'a tree = leaf of 'a | node of 'a tree list

(* val myTree = node [node [node [leaf [4,2,14], leaf [9,83,32], leaf [96,123,4]], node [leaf [47,71,82]], node [leaf [19,27,10], leaf [111,77,22,66]], leaf [120,42,16]], leaf [83,13]] *)



(* QUESTION 8 *)
(* Define a function fringe t, where t is an ’a tree (above), which computes the fringe of t. The fringe of a tree is a list of the values at the leaves of a tree. You should use your reduce function (along with map), allowing fringe to be written in roughly three lines. *)

(* fringe without reduce and map *)
fun fringe (node []) = []
| fringe (leaf x) = [x]
| fringe (node (x::xs)) = fringe x @ fringe (node xs)

(* fringe (node [leaf 1,node [leaf 2,leaf 3],node [node [leaf 4,leaf 5],leaf 6]]) *)
(* fringe myTree *)

(* fringe with reduce and map *)

fun fringe (node []) = []
| fringe (leaf x) = [x]
| fringe (node y) = 
	reduce (fn a => fn b => a @ b) (map fringe y)



(* QUESTION 9 *)
(* Write a polymorphic function sortTree (op <) t, where t is of type ’a list tree, that returns a tree with the same structure as t, except that the list found at each leaf has been sorted using your pSort function, above. *)

fun sortTree (op <) (leaf x) = leaf (pSort (op <) x) 
| sortTree (op <) (node n) = 
	node (map (sortTree (op <)) n)

(* sortTree (op <) myTree *)



(* QUESTION 10 *)
(* Suppose that a set is represented in ML as a list. Define the function powerSet L, where L is of type ’a list, that returns the power set of L. That is, it returns the list of all possible sub-lists of L. *)

(* using inbuilt map function *)
fun powerSet [] = []
| powerSet [x] = [[],[x]]
| powerSet (x::xs) =
	let
		val p = powerSet xs
	in
		(map (fn i => x::i) p) @ p
	end

(* without using map *)
fun powerSet [] = []
| powerSet [x] = [[],[x]]
| powerSet (x::xs) =
	let
		val p = powerSet xs
		fun add a nil = nil
		| add a (l::ls) = (a::l) :: add a ls
	in
		(add x p) @ p
	end

(* powerSet [1,2,3] *)