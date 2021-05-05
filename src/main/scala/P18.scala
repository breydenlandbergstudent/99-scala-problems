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
    def sliceInbuilt[A] (K : Int, L : Int, list : List[A]) : Option[List[A]] = {
        Some (list.slice (K, L))
    }

    def sliceOriginal[_] (K : Int, L : Int, list : List[_]) : Option[List[_]] = {
        @tailrec
        def innerSlice[A] (sliced : List[A], innerK : Int, innerL : Int, innerList : List[A]) : Option[List[A]] = {
            innerList match {
                case Nil => Some (sliced)
                case _ if innerK == 0 && innerL == 0 => Some (sliced)
                case _ :: tl if innerK > 0 && innerL > innerK => innerSlice (sliced, innerK - 1, innerL - 1, tl)
                case hd :: tl => innerSlice (sliced :+ hd, innerK, innerL - 1, tl)
            }
        }
        innerSlice (List (), K, L, list)
    }

    // more idiomatic than original solution above
    def slice[_] (K : Int, L : Int, list : List[_]) : Option[List[_]] = {
        @tailrec
        def innerSlice[A] (sliced : List[A], innerList : List[A]) : Option[List[A]] = {
            innerList match {
                case Nil => Some (sliced)
                case _ :: tl if list.length - innerList.length < K || list.length - innerList.length >= L => innerSlice (sliced, tl)
                case hd :: tl => innerSlice (sliced :+ hd, tl)
            }
        }
        innerSlice (List (), list)
    }

    // an alternative solution...
    def sliceAlternative[_] (K : Int, L : Int, list : List[_]): Option[List[_]] = {
        @tailrec
        def innerSlice[A] (idx : Int, unsliced : List[A], sliced : List[A]) : Option[List[A]] = {
            unsliced match {
                case Nil => Some (sliced)
                case hd :: tl =>
                    if (idx >= K && idx < L) {
                        val newSliced = sliced :+ hd
                        val newIdx = idx + 1
                        innerSlice (newIdx, tl, newSliced)
                    }
                    else {
                        innerSlice (idx + 1, tl, sliced)
                    }
            }
        }
        if (K <= 0 || L > list.size) {
            Some (List ())
        }
        else {
            innerSlice(0, list, List())
        }
    }

    println ("P18 solution using inbuilt collection API method given K = 2, L = 4, list = List (0, 1, 2, 3, 4, 5) is:   " + sliceInbuilt (2, 4, List (0, 1, 2, 3, 4, 5)))
    println ("P18 solution using inbuilt collection API given K = 5, L = 11, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) is:   "
        + sliceOriginal (5, 11, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)))
    println ("P18 solution using inbuilt collection API method given K = 2, L = 4, list = List () is:   " + sliceInbuilt (2, 4, List ()))

    println

    println ("P18 original solution given K = 2, L = 4, list = List (0, 1, 2, 3, 4, 5) is:   " + sliceOriginal (2, 4, List (0, 1, 2, 3, 4, 5)))
    println ("P18 original solution given K = 5, L = 11, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) is:   "
        + sliceOriginal (5, 11, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)))
    println ("P18 original solution given K = 2, L = 4, list = List () is:   " + sliceOriginal (2, 4, List ()))

    println

    println ("P18 solution given K = 2, L = 4, list = List (0, 1, 2, 3, 4, 5) is:   " + slice (2, 4, List (0, 1, 2, 3, 4, 5)))
    println ("P18 solution given K = 5, L = 11, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) is:   "
        + slice (5, 11, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)))
    println ("P18 solution given K = 2, L = 4, list = List () is:   " + slice (2, 4, List ()))

    println

    println ("P18 alternative solution given K = 2, L = 4, list = List (0, 1, 2, 3, 4, 5) is:   " + sliceAlternative (2, 4, List (0, 1, 2, 3, 4, 5)))
    println ("P18 alternative solution given K = 5, L = 11, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) is:   "
        + sliceAlternative (5, 11, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)))
    println ("P18 alternative solution given K = 2, L = 4, list = List () is:   " + sliceAlternative (2, 4, List ()))

    println
}