import scala.annotation.tailrec

// Problem 19
// (**) Rotate a list N places to the left.
//
//
// Examples:
//
// scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
//
// scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

object P19 extends App {
    def rotateLeft (N : Int, list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerRotateLeft (rotated: List[Any], innerList: List[Any]): Option[List[Any]] = {
            N match {
                case N if N == 0 => None
                case N if N > 0 =>
                    innerList match {
                        case Nil => Some (rotated)
                        case hd :: Nil => Some (rotated ::: List(hd))
                        case _ :: tl if innerList.length > N => innerRotateLeft (List (tl.last) ::: rotated, innerList.init)
                        case hd :: tl => innerRotateLeft (rotated ::: List(hd), tl)
                    }
                case N if N < 0 =>
                    innerList match {
                        case Nil => Some (rotated)
                        case hd :: Nil => Some (rotated ::: List(hd))
                        case _ :: tl if (innerList.length * -1) < ((list.length * -1) - N) => innerRotateLeft (List (tl.last) ::: rotated, innerList.init)
                        case hd :: tl => innerRotateLeft (rotated ::: List (hd), tl)
                    }
            }
        }
        innerRotateLeft (List (), list)
    }

    println ("P19 solution given N = 3, list = List (0, 1, 2, 3, 4, 5) is:   " + rotateLeft (3, List (0, 1, 2, 3, 4, 5)))
    println ("P19 solution given N = 5, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) is:   " + rotateLeft (5, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
    println ("P19 solution given N = 7, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) is:   " + rotateLeft (7, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
    println ("P19 solution given N = -3, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) is:   " + rotateLeft (-3, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
    println ("P19 solution given N = -7, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) is:   " + rotateLeft (-7, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
    println ("P19 solution given N = 3, list = List () is:   " + rotateLeft (3, List ()))
}