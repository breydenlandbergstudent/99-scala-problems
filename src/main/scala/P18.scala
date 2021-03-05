import scala.annotation.tailrec

// Problem 18
// (**) Extract a slice from a list.
// Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list.
// Start counting the elements with 0.
//
//
// Example:
//
// scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('d, 'e, 'f, 'g)

// not finished
// TODO
object P18 extends App {
    def sliceInbuilt (K : Int, L : Int, list : List[Any]) : Option[List[Any]] = {
        Some (list slice (K, L))
    }

    // uses an inner counter mechanism (not very idiomatic)
    def slice (K : Int, L : Int, list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerSlice (sliced : List[Any], innerK : Int, innerL : Int, innerList : List[Any]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (sliced)
                case _ if innerK == 0 && innerL == 0 => Some (sliced)
                case _ :: tl if innerK > 0 && innerL > innerK => innerSlice (sliced, innerK - 1, innerL - 1, tl)
                case hd :: tl => innerSlice (sliced ::: List (hd), innerK, innerL - 1, tl)
            }
        }
        innerSlice (List (), K, L, list)
    }

    println ("P18 solution using inbuilt collection API method given K = 2, L = 4, list = List (0, 1, 2, 3, 4, 5)) is:   " + sliceInbuilt (2, 4, List (0, 1, 2, 3, 4, 5)))
    println ("P18 solution given K = 2, L = 4, list = List (0, 1, 2, 3, 4, 5)) is:   " + slice (2, 4, List (0, 1, 2, 3, 4, 5)))
    println ("P18 solution using inbuilt collection API method given K = 2, L = 4, list = List ()) is:   " + sliceInbuilt (2, 4, List ()))
    println ("P18 solution given K = 2, L = 4, list = List ()) is:   " + slice (2, 4, List ()))
}