import scala.annotation.tailrec

// Problem 9
// (**) Pack consecutive duplicates of list elements into sub-lists.
// If a list contains repeated elements they should be placed in separate sublists.
//
//
// Example:
//
// scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

object P09 extends App {
    def pack (list : List[Any]) : Option[List[List[Any]]] = {
        @tailrec
        def innerPack (packed : List[List[Any]], innerList : List[Any]) : Option[List[List[Any]]] = {
            innerList match {
                case Nil => Some (packed)
                case hd :: tl if packed.nonEmpty && hd == packed.last.head => innerPack (packed.init ::: List (packed.last ::: List (hd)), tl)
                case hd :: tl => innerPack (packed ::: List (List (hd)), tl)
            }
        }
        innerPack (List (), list)
    }

    println ("P09 solution given List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'd', 'f', 'd', 'd') is:   "
        + P09.pack (List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'd', 'f', 'd', 'd')))
    println ("P09 solution given List () is:   " + P09.pack (List ()))
}