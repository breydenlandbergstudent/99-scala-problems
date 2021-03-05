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
    def duplicateOriginal (list : List[Any]) : List[Any] = {
        list match {
            case Nil => Nil
            case hd :: Nil => List (hd, hd)
            case hd :: tl => List (hd, hd) ::: duplicateOriginal (tl)
        }
    }

    def duplicate (list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerDuplicate (duplicated : List[Any], innerList : List[Any]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (duplicated)
                case hd :: tl => innerDuplicate (duplicated ::: List.fill (2) (hd), tl)
            }
        }
        innerDuplicate (List (), list)
    }

    println ("P14 original solution given List ('a', 'b', 'c', 'd', 'e', 'f') is:   " + duplicateOriginal (List ('a', 'b', 'c', 'd', 'e', 'f')))
    println ("P14 solution given List ('a', 'b', 'c', 'd', 'e', 'f') is:   " + duplicateOriginal (List ('a', 'b', 'c', 'd', 'e', 'f')))
    println ("P14 original solution given List () is:   " + duplicate (List ()))
    println ("P14 solution given List () is:   " + duplicateOriginal (List ()))
}