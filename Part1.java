import java.util.*;

/*
STEP 1:
Define a generic class SortedList<> that implements both the List and Comparable interfaces, such that two objects of type SortedList<> can be compared using the compareTo method. You can extend a built-in generic class, e.g. ArrayList, if you want, but that is up to you (it will make things easier).
The elements of SortedList must remain in sorted order. Therefore, the element type must also implement the Comparable interface.
The SortedList class should implement an add method (e.g. by overriding the add() method of ArrayList), so that any new element added to a SortedList is added to the right place in the list in order to keep the SortedList in sorted order (instead of just being added at the end of the list).
The comparison method, compareTo(), required by the Comparable interface, should take another SortedList<T> object (for the same T) and perform a lexicographic comparison.   Based on a lexicographic comparison, a list L1 is less than a list L2 if one of the following is true:
- there is a k such that the first k-1 elements of L1 and L2 are equal and the kth element of L1 is less than the kth element of L2, or
- L1 is of length k1 and L2 is of length k2, such that k1 < k2 and the first k1 elements of the two lists are equal.
For example, [1,2,3,4,5,8] < [1,2,3,4,6,7] and [2,4,6,8] < [2,4,6,8,10]. Note that we use a lexicographic comparison to order words alphabetically.
You may want to override the toString() method, if you don't like the way your SortedList objects print out.  
 */
class SortedList<T extends Comparable<T>> extends ArrayList<T> implements Comparable<SortedList<T>>, List<T> {

    @Override
    public boolean add(T el) {
        for (int i = 0; i < this.size(); i++) {
            T z = this.get(i);
            if (z.compareTo(el) >= 0) {
                super.add(i, el);
                return true;
            }
        }
        return super.add(el);
    }

    @Override
    public int compareTo(SortedList<T> l) {
        int i;
        for (i = 0; i < this.size() && i < l.size(); i++) {
            if (this.get(i).compareTo(l.get(i)) == 0) {
                continue;
            } else if (this.get(i).compareTo(l.get(i)) < 0) {
                return -1;
            } else {
                return 1;
            }
        }
        if (i == this.size() && i == l.size()) {
            return 0;
        } else if (i == l.size()) {
            return 1;
        } else if (i == this.size()) {
            return -1;
        }
        return 100;
    }

    @Override
    public String toString() {
        Iterator<T> myIt = this.iterator();
        String str = "";
        while (myIt.hasNext()) {
            Object o = myIt.next();
            str += o.toString();
            if (myIt.hasNext()) {
                str = str + " ";
            }
        }
        return "[" + str + "]";
    }
}

/*
STEP 2:
Define a class A that can be used to instantiate SortedList<A>, which also means that two A's must be able to be compared to each other. You can define A any way you like, the only requirements are:
A includes a constructor, A(Integer x) {...}.
When comparing two A objects, the result of the comparison should be based on the x values that each object was initially constructed with. That is, given
    A a1 = new A(4);
    A a2 = new A(5);
the result of a1.compareTo(a2) should return -1, indicating that a1 is less than a2 (because 4 < 5).
You'll also want to override the toString() method, so A objects print nicely.
 */
class A implements Comparable<A> {

    Integer val;

    A() {
    }

    A(Integer x) {
        this.val = x;
    }

    @Override
    public String toString() {
        return "A<" + this.val + ">";
    }

    @Override
    public int compareTo(A el) {
        if (this.val < el.val) {
            return -1;
        } else if (this.val > el.val) {
            return 1;
        }
        return 0;
    }
}

/*
STEP 3:
Define a class B that extends A and overrides the inherited compareTo() method. You can define B any way you like, the only requirements are:
B includes a constructor, B(Integer x, Integer y) {...}.
the compareTo method should be overridden so that the value of x+y is used as the basis for comparison. For example, given
    A a1 = new A(6);
    B b1 = new B(2,4);
    B b2 = new B(5,8);
the results of the comparisons should be:
    a1.compareTo(b1);  //returns 0, since 6 = (2+4)
    a1.compareTo(b2);  //returns -1, since 6 < (5+8)
    b1.compareTo(a1);  //returns 0, since (2+4) = 6
    b2.compareTo(a1);  //returns 1, since (5+8) > (6)
    b1.compareTo(b2);  //returns -1, since (2+4) < (5+8)
You'll also want to override the toString() method, so B objects print nicely.
 */
class B extends A {

    Integer val1, val2;

    B(Integer x, Integer y) {
        super(x + y);
        this.val1 = x;
        this.val2 = y;
    }

    @Override
    public String toString() {
        return "B<" + this.val1 + "," + this.val2 + ">";
    }

    @Override
    public int compareTo(A el) {
        return super.compareTo(el);
    }
}

/*
STEP 4:
In a separate class named Part1, define the static main() method. In that same class, define a static method addtoSortedList() that is polymorphic over any type T and which takes two parameters, z and L, where z is of type T and L can be any SortedList into which an object of type T can be inserted.   addtoSortedList() should add z to L (which will automatically remain sorted, of course).
Have main() call this test() method. The result should look something like:
c1: [[A<0> A<5> A<10> A<15> A<20> A<25> A<30> A<35>]]
c2: [[B<2,3> B<7,8> B<12,13> B<17,18> B<22,23> B<27,28> B<32,33> B<37,38>]]
c1 < c2
 */
public class Part1 {

    static <T extends Comparable<T>> void addToSortedList(SortedList<T> l, T z) {
        l.add(z);
    }

    static void test() {
        SortedList<A> c1 = new SortedList<A>();
        SortedList<A> c2 = new SortedList<A>();

        for (int i = 35; i >= 0; i -= 5) {
            addToSortedList(c1, new A(i));
            addToSortedList(c2, new B(i + 2, i + 3));
        }

        System.out.print("c1: ");
        System.out.println(c1);

        System.out.print("c2: ");
        System.out.println(c2);

        switch (c1.compareTo(c2)) {
            case -1:
                System.out.println("c1 < c2");
                break;
            case 0:
                System.out.println("c1 = c2");
                break;
            case 1:
                System.out.println("c1 > c2");
                break;
            default:
                System.out.println("Uh Oh");
                break;
        }
    }

    public static void main(String[] args) {
        test();
    }
}
