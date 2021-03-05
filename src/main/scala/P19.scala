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

// not finished
// TODO
object P19 extends App {
    // uses an inner counter mechanism (not very idiomatic)
    def rotateN (N : Int, list : List[Any]) : Option[List[Any]] = {
        def innerRotateN (rotated : List[Any], innerN : Int, innerList : List[Any]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (rotated)
                case hd :: tl => ???
            }
        }
        innerRotateN (List (), N, list)
    }

    println ("P19 solution given N = 3, list = List (0, 1, 2, 3, 4, 5)) is:   " + rotateN (3, List (0, 1, 2, 3, 4, 5)))
    println ("P19 solution given N = 3, list = List ()) is:   " + rotateN (3, List ()))
}