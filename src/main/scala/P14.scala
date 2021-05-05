import scala.annotation.tailrec

// Problem 14
// (*) Duplicate the elements of a list.
//
//
// Example:
//
// scala> duplicate(List('a, 'b, 'c, 'c, 'd))
// res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

object P14 extends App {
    def duplicateOriginal[A] (list : List[A]) : List[A] = {
        list match {
            case Nil => Nil
            case hd :: Nil => List (hd, hd)
            case hd :: tl => List (hd, hd) ::: duplicateOriginal (tl)
        }
    }

    def duplicate[_] (list : List[_]) : Option[List[_]] = {
        @tailrec
        def innerDuplicate[A] (duplicated : List[A], innerList : List[A]) : Option[List[A]] = {
            innerList match {
                case Nil => Some (duplicated)
                case hd :: tl => innerDuplicate (duplicated ::: List.fill (2) (hd), tl)
            }
        }
        innerDuplicate (List (), list)
    }

    println ("P14 original solution given List ('a', 'b', 'c', 'd', 'e', 'f') is:   " + duplicateOriginal (List ('a', 'b', 'c', 'd', 'e', 'f')))
    println ("P14 solution given List () is:   " + duplicateOriginal (List ()))

    println

    println ("P14 solution given List ('a', 'b', 'c', 'd', 'e', 'f') is:   " + duplicate (List ('a', 'b', 'c', 'd', 'e', 'f')))
    println ("P14 original solution given List () is:   " + duplicate (List ()))

    println
}