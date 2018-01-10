/* Define a generic abstract class Tree, parameterized by a type T, such that:
- A generic case class Node, also parameterized by type T, extends Tree[T] and represents an interior node that has a label of type T, a left subtree, and a right subtree (both of type Tree[T]).
- A generic case class Leaf, parameterized by type T, extends Tree[T] and represents a leaf that has a label of type T.
Tree[T] is covariantly subtyped. That is, if B is a subtype of A, then Tree[B] is a subtype of Tree[A]. */

abstract class Tree[+T] {

}

case class Leaf[T](x: T) extends Tree[T] {
	override def toString(): String = "Leaf(" + x + ")"
}

case class Node[T](x: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
	override def toString(): String = "Node(" + x + "," + left.toString() + "," + right.toString() + ")"
}

/* Define a generic trait Addable, parameterized by a type T, that requires any class implementing the Addable trait to have a + method that takes a parameter of type T and returns a result of type T. */

trait Addable[T] {
	def +(x: T): T
}

/* Define a class A that implements the Addable trait, such that:
- An A object is constructed using an integer parameter, e.g. new A(6). That integer should be stored within the A object.
- The result of adding two A operands together is an A object constructed with the sum of the integers within the two operand objects. For example, new A(6) + new A(7) would create a new A object with the parameter 13.
- The toString() method of A is overridden to show the integer value stored within it as well as to indicate that the object is of type A. */

class A(x: Int) extends Addable[A] {
	def value: Int = x
	def +(obj: A): A = new A(this.value + obj.value)
	override def toString(): String = "A(" + this.value + ")"
}

/* Define a class B that extends A, such that B also takes an integer parameter and overrides the toString() method to show the integer and to indicate that the object is a B. */

class B(x: Int) extends A(x) {
	override def toString(): String = "B(" + this.value + ")"
}

/* Define a class C that extends B, such that C also takes an integer parameter and overrides the toString() method to show the integer and to indicate that the object is a C. */

class C(x: Int) extends B(x) {
	override def toString(): String = "C(" + this.value + ")"
}

/* In a singleton class named Part2, put the following:
- A generic function, inOrder, that is parameterized by type T and computes the list of labels found in a tree, in in-order order. inOrder should take a Tree[T] as a parameter and return a List[T] as the result (where List[] is a generic class defined in the Scala API).
- A generic function treeSum, parameterized by type T such that any such T has to implement the Addable trait, which computes the sum of all the labels in a tree. It should take a Tree[T] as a parameter and and return a T as the result.
- A generic function treeMap (analogous to MAP in Scheme or ML) which applies a function to every label in a tree, returning a tree of the results. treeMap, for any types T and V, should take a function of type T=>V and a tree of type Tree[T] as parameters and return a tree of type Tree[V] as a result. The resulting tree should have the same structure (relationship of parent and child nodes) as the original tree.
- A function BTreeMap that takes a function of type B=>B and a tree of type Tree[B] and (just like for TreeMap, above) applies the function to every label in the tree, returning a tree of type Tree[B] as the result. */

object Part2 {
	import scala.collection.mutable._

	def inOrder[T](inp: Tree[T]): List[T] = {
		var op = new ListBuffer[T]()
		def in_rec[T](in: Tree[T], op: ListBuffer[T]): ListBuffer[T] = {
			in match {
				case Node(n, l ,r) => 
					in_rec(l,op)
					op += n
					in_rec(r,op)

				case Leaf(n) =>
					op += n
			}
			op
		}
		in_rec(inp,op).toList
	}


	def treeSum[T <: Addable[T]](inp: Tree[T]): T = {
		inp match {
			case Leaf(n) => n

			case Node(n, l, r) => n + treeSum(l) + treeSum(r)
		}
	}

	def treeMap[T,V](func: T => V, inp: Tree[T]): Tree[V] = {
		inp match {
			case Leaf(n) => new Leaf(func(n))

			case Node(n, l, r) => new Node(func(n), treeMap(func, l), treeMap(func, r)) 
		}
	}

	def BTreeMap(func: B => B, inp: Tree[B]): Tree[B] = {
		inp match {
			case Leaf(n) => new Leaf(func(n))

			case Node(n, l, r) => new Node(func(n), treeMap(func, l), treeMap(func, r)) 
		}		
	}

	def test() {
		def faa(a: A): A = new A(a.value + 10)
	    def fab(a: A): B = new B(a.value + 20)
	    def fba(b: B): A = new A(b.value + 30)
	    def fbb(b: B): B = new B(b.value + 40)
	    def fbc(b: B): C = new C(b.value + 50)
	    def fcb(c: C): B = new B(c.value + 60)
	    def fcc(c: C): C = new C(c.value + 70)
	    def fac(a: A): C = new C(a.value + 80)
   		def fca(c: C): A = new A(c.value + 90)

		val myBTree: Tree[B] = Node(new B(4), Node(new B(2), Leaf(new B(1)), Leaf(new B(3))), Node(new B(6), Leaf(new B(5)), Leaf(new B(7))))
    	val myATree: Tree[A] = myBTree

    	println("inOrder = " + inOrder(myATree))
    	println("Sum = " + treeSum(myATree))

    	// println(BTreeMap(faa,myBTree)) gives an error because BTreeMap expects a function of type B => B, and since it is covariant in output type, it cannot accept a function that is of type A => A 
	    println(BTreeMap(fab,myBTree))
	    // println(BTreeMap(fba,myBTree)) gives an error because function subtyping is covariant in output type, so the output of the function that is passed cannot be of type A
	    println(BTreeMap(fbb,myBTree))
	    println(BTreeMap(fbc,myBTree))
	    // println(BTreeMap(fcb,myBTree)) gives an error because function subtyping is contravariant in input type, so the input of the function that is passed cannot be of type C
	    // println(BTreeMap(fcc,myBTree)) gives an error because function subtyping is contravariant in input type, so the input of the function that is passed cannot be of type C
	    println(BTreeMap(fac,myBTree))
	    // println(BTreeMap(fca,myBTree)) gives an error because function subtyping is contravariant in input type, so the input of the function that is passed cannot be of type C, and function subtyping is covariant in output type, so the output of the function that is passed cannot be of type A

	    println(treeMap(faa,myATree))
	    println(treeMap(fab,myATree))
	    // println(treeMap(fba,myATree)) gives an error because the treeMap function expects the type of the tree input to be of the input type of the function that is passed in treeMap, so it cannot accept a tree of type A if the function that is passed is of type B => A
	    // println(treeMap(fbb,myATree)) gives an error because the treeMap function expects the type of the tree input to be of the input type of the function that is passed in treeMap, so it cannot accept a tree of type A if the function that is passed is of type B => B
	    // println(treeMap(fbc,myATree)) gives an error because the treeMap function expects the type of the tree input to be of the input type of the function that is passed in treeMap, so it cannot accept a tree of type A if the function that is passed is of type B => C
	    // println(treeMap(fcb,myATree)) gives an error because the treeMap function expects the type of the tree input to be of the input type of the function that is passed in treeMap, so it cannot accept a tree of type A if the function that is passed is of type C => B
	    // println(treeMap(fcc,myATree)) gives an error because the treeMap function expects the type of the tree input to be of the input type of the function that is passed in treeMap, so it cannot accept a tree of type A if the function that is passed is of type A => C
	    println(treeMap(fac,myATree))
	    // println(treeMap(fca,myATree)) gives an error because the treeMap function expects the type of the tree input to be of the input type of the function that is passed in treeMap, so it cannot accept a tree of type A if the function that is passed is of type A => C
	}

	def main(args: Array[String]): Unit = {
		test()
	}
}