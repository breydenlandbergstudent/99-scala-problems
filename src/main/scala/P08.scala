// Problem 8
// (**) Eliminate consecutive duplicates of list elements.
// If a list contains repeated elements they should be replaced with a single copy of the element.
// The order of the elements should not be changed.
//
//
// Example:
//
// scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

object P08 extends App {
    def compress (list : List[Any]) : List[Any] = {
        list match {
            case Nil => Nil
            case hd :: Nil => List (hd)
            case hd :: tl if hd == tl.head => compress (tl)
            case hd :: tl => List (hd) ::: compress (tl)
        }
    }

    println ("P08 solution given list = List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e') is:   "
        + compress (List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e')))
    println ("P08 solution given list = List () is:   " + compress (List ()))
}