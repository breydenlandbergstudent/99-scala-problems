import scala.annotation.tailrec

// Problem 16
// (**) Drop every Nth element from a list.
//
//
// Example:
//
// scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

object P16 extends App {
    def dropN[_] (N : Int, list : List[_]) : Option[List[_]] = {
        @tailrec
        def innerDropN[A] (dropped : List[A], innerList : List[A]) : Option[List[A]] = {
            innerList match {
                case Nil => Some (dropped)
                case _ :: tl if (list.length - innerList.length + 1) % N == 1 => innerDropN (dropped, tl)
                case hd :: tl => innerDropN (dropped :+ hd , tl)
            }
        }
        innerDropN (List (), list)
    }

    def dropNAlternative[_] (N : Int, list : List[_]) : Option[List[_]] = {
        @tailrec
        def innerDropN[A] (idx : Int, dropped : List[A], undropped : List[A]) : Option[List[A]] = {
            undropped match {
                case Nil => Some (dropped)
                case hd :: tl => {
                    if (idx % N == 0) {
                        innerDropN (idx + 1, dropped, tl)
                    }
                    else {
                        innerDropN (idx + 1, dropped :+ hd, tl)
                    }
                }
            }
        }
        innerDropN (1, List (), list)
    }

    println ("P16 solution given N = 3, list = List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f') is:   "
        + dropN (3, List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f')))
    println ("P16 solution given N = -3, list = List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f') is:   "
        + dropNAlternative (-3, List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f')))
    println ("P16 solution given N = 3, list = List () is:   " + dropN (3, List ()))

    println

    println ("P16 alternative solution given N = 3, list = List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f') is:   "
        + dropNAlternative (3, List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f')))
    println ("P16 alternative solution given N = -3, list = List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f') is:   "
        + dropNAlternative (-3, List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f')))
    println ("P16 alternative solution given N = 3, list = List () is:   " + dropNAlternative (3, List ()))

    println
}