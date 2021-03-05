import scala.annotation.tailrec

// Problem 21
// (*) Insert an element at a given position into a list.
//
//
// Example:
//
// scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
// res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

// not finished
// TODO
object P21 extends App {
    // not finished
    // TODO
    // uses an inner counter mechanism (not very idiomatic)
    def insert (item : Any, K : Int, list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerInsert (runningList : List[Any], innerK : Int, innerList : List[Any]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (runningList)
                case hd :: tl if innerK == 0 => innerInsert (runningList ::: List (item) ::: List (hd), innerK - 1, tl)
                case hd :: tl if innerK > 0 => innerInsert (runningList ::: List (hd), innerK - 1, tl)
                case hd :: tl if innerK < 0 => innerInsert (runningList ::: List (hd), innerK - 1, tl)
            }
        }
        innerInsert (List (), K, list)
    }

    println ("P21 solution given item = \"inserted\", K = 4, list = List (0, 1, 2, 3, 4, 5)) is:   " + insert ("inserted", 4, List (0, 1, 2, 3, 4, 5)))
    println ("P21 solution given item = \"inserted\", K = 4, list = List ()) is:   " + insert ("inserted", 4, List ()))
}