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

// not finished
// TODO
object P20 extends App {
    // not finished
    // TODO
    // uses an inner counter mechanism (not very idiomatic)
    def remove (K : Int, list : List[Any]) : Option[(List[Any], Any)] = {
        @tailrec
        def innerRemove (removed : (List[Any], Any), innerK : Int, innerList : List[Any]) : Option[(List[Any], Any)] = {
            innerList match {
                case Nil => Some (removed)
                case hd :: tl if innerK > 0 => innerRemove ((removed._1 ::: List (hd), removed._2), innerK - 1, tl)
                case hd :: tl if innerK == 0 => innerRemove ((removed._1, hd), innerK - 1, tl)
                case hd :: tl if innerK < 0 => innerRemove ((removed._1 ::: List (hd), removed._2), innerK - 1, tl)
            }
        }
        innerRemove ((List (), Nil), K, list)
    }

    println ("P20 solution given K = 3, list = List (0, 1, 2, 3, 4, 5)) is:   " + remove (3, List (0, 1, 2, 3, 4, 5)))
    println ("P20 solution given K = 3, list = List ()) is:   " + remove (3, List ()))
}