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
    // uses an inner counter mechanism (not very idiomatic)
    def duplicateNOriginal (N : Int, list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerDuplicateN (duplicated: List[Any], innerN : Int, innerList : List[Any]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (duplicated)
                case hd :: tl if innerN == 1 => innerDuplicateN (duplicated ::: List (hd), N, tl)
                case hd :: tl => innerDuplicateN (duplicated ::: List (hd), innerN - 1, hd :: tl)
            }
        }
        innerDuplicateN (List (), N, list)
    }

    def duplicateN (N : Int, list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerDuplicateN (duplicated : List[Any], innerList : List[Any]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (duplicated)
                case hd :: tl => innerDuplicateN (duplicated ::: List.fill (N) (hd), tl)
            }
        }
        innerDuplicateN (List (), list)
    }

    println ("P15 original solution given N = 5, List (a, b, c, d, e, f)) is:   " + P15.duplicateNOriginal (5, List ('a', 'b', 'c', 'd', 'e', 'f')))
    println ("P15 solution given N = 5, List (a, b, c, d, e, f)) is:   " + P15.duplicateN (5, List ('a', 'b', 'c', 'd', 'e', 'f')))
    println ("P15 original solution given N = 5, List ()) is:   " + P15.duplicateNOriginal (5, List ()))
    println ("P15 solution given N = 5, List ()) is:   " + P15.duplicateN (5, List ()))
}