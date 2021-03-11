import scala.annotation.tailrec

// Problem 18
// (**) Extract a slice from a list.
// Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list.
// Start counting the elements with 0.
//
// * - - - indices in this implementation are K and L - - - *
//
// Example:
//
// scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('d, 'e, 'f, 'g)

object P18 extends App {
    def sliceInbuilt (K : Int, L : Int, list : List[Any]) : Option[List[Any]] = {
        Some (list slice (K, L))
    }

    // uses an inner counter mechanism (not an optimal solution)
    def sliceOriginal (K : Int, L : Int, list : List[Any]) : Option[List[Any]] = {
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

    def slice (K : Int, L : Int, list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerSlice (sliced : List[Any], innerList : List[Any]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (sliced)
                case _ :: tl if list.length - innerList.length < K || list.length - innerList.length >= L => innerSlice (sliced, tl)
                case hd :: tl => innerSlice (sliced ::: List (hd), tl)
            }
        }
        innerSlice (List (), list)
    }

    println ("P18 solution using inbuilt collection API method given K = 2, L = 4, list = List (0, 1, 2, 3, 4, 5)) is:   " + sliceInbuilt (2, 4, List (0, 1, 2, 3, 4, 5)))
    println ("P18 original solution given K = 2, L = 4, list = List (0, 1, 2, 3, 4, 5)) is:   " + sliceOriginal (2, 4, List (0, 1, 2, 3, 4, 5)))
    println ("P18 original solution given K = 5, L = 11, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)) is:   " + sliceOriginal (5, 11, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)))
    println ("P18 solution given K = 2, L = 4, list = List (0, 1, 2, 3, 4, 5)) is:   " + slice (2, 4, List (0, 1, 2, 3, 4, 5)))
    println ("P18 solution given K = 5, L = 11, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)) is:   " + slice (5, 11, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)))
    println ("P18 solution using inbuilt collection API method given K = 2, L = 4, list = List ()) is:   " + sliceInbuilt (2, 4, List ()))
    println ("P18 original solution given K = 2, L = 4, list = List ()) is:   " + sliceOriginal (2, 4, List ()))
}