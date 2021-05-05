import scala.annotation.tailrec

// Problem 15
// (**) Duplicate the elements of a list a given number of times.
//
//
// Example:
//
// scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
// res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

object P15 extends App {
    def duplicateNOriginal[_] (N : Int, list : List[_]) : Option[List[_]] = {
        @tailrec
        def innerDuplicateN[A] (duplicated: List[A], innerN : Int, innerList : List[A]) : Option[List[A]] = {
            innerList match {
                case Nil => Some (duplicated)
                case hd :: tl if innerN == 1 => innerDuplicateN (duplicated :+ hd, N, tl)
                case hd :: tl => innerDuplicateN (duplicated :+ hd, innerN - 1, hd :: tl)
            }
        }
        innerDuplicateN (List (), N, list)
    }

    // more idiomatic than original solution above
    def duplicateN[_] (N : Int, list : List[_]) : Option[List[_]] = {
        @tailrec
        def innerDuplicateN[A] (duplicated : List[A], innerList : List[A]) : Option[List[A]] = {
            innerList match {
                case Nil => Some (duplicated)
                case hd :: tl => innerDuplicateN (duplicated ::: List.fill (N) (hd), tl)
            }
        }
        innerDuplicateN (List (), list)
    }

    println ("P15 original solution given N = 5, List (a, b, c, d, e, f) is:   " + P15.duplicateNOriginal (5, List ('a', 'b', 'c', 'd', 'e', 'f')))
    println ("P15 original solution given N = 5, List () is:   " + P15.duplicateNOriginal (5, List ()))

    println

    println ("P15 solution given N = 5, List (a, b, c, d, e, f) is:   " + P15.duplicateN (5, List ('a', 'b', 'c', 'd', 'e', 'f')))
    println ("P15 solution given N = 5, List () is:   " + P15.duplicateN (5, List ()))

    println
}