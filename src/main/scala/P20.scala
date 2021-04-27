import scala.annotation.tailrec

// Problem 20
// (*) Remove the Kth element from a list.
// Return the list and the removed element in a Tuple. Elements are numbered from 0.
//
//
// Example:
//
// scala> removeAt(1, List('a, 'b, 'c, 'd))
// res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

object P20 extends App {
    // uses an inner counter mechanism (not very idiomatic)
    def removeOriginal (K : Int, list : List[Any]) : Option[(List[Any], Any)] = {
        @tailrec
        def innerRemove (removed : (List[Any], Any), innerK : Int, innerList : List[Any]) : Option[(List[Any], Any)] = {
            innerList match {
                case Nil => Some (removed)
                case hd :: tl if innerK == 0 => innerRemove ((removed._1, hd), innerK - 1, tl)
                case hd :: tl => innerRemove ((removed._1 ::: List (hd), removed._2), innerK - 1, tl)
            }
        }
        innerRemove ((List (), Nil), K, list)
    }

    // could use if/else?
    def remove (K : Int, list : List[Any]) : Option[(List[Any], Any)] = {
        @tailrec
        def innerRemove (removed : (List[Any], Any), innerList : List[Any]) : Option[(List[Any], Any)] = {
            innerList match {
                case Nil => Some (removed)
                case hd :: tl if list.length - innerList.length == K => innerRemove ((removed._1, hd), tl)
                case hd :: tl => innerRemove ((removed._1 ::: List (hd), removed._2), tl)
            }
        }
        innerRemove ((List (), Nil), list)
    }

    println ("P20 original solution given K = 3, list = List (0, 1, 2, 3, 4, 5) is:   " + removeOriginal (3, List (0, 1, 2, 3, 4, 5)))
    println ("P20 solution given K = 3, list = List (0, 1, 2, 3, 4, 5) is:   " + remove (3, List (0, 1, 2, 3, 4, 5)))
    println ("P20 original solution given K = 7, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) is:   " + removeOriginal (7, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
    println ("P20 solution given K = 7, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) is:   " + remove (7, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
    println ("P20 original solution given K = 3, list = List () is:   " + removeOriginal (3, List ()))
    println ("P20 solution given K = 3, list = List () is:   " + remove (3, List ()))
}