// Problem 7
// (**) Flatten a nested list structure.
//
//
// Example:
//
// scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
// res0: List[Any] = List(1, 1, 2, 3, 5, 8)

object P07 extends App {
    def flatten (list : List[Any]) : List[Any] = {
        list match {
            case Nil => Nil
            case (hd : List[Any]) :: tl => flatten (hd) ::: flatten (tl)
            case (hd : Any) :: tl => hd :: flatten (tl)
        }
    }

    println ("P07 solution given list = List (List (0, 1, 2, 3), 0, 1, 2, 3, List (4, List (5, List (6)))) is:   " + flatten (List (List (0, 1, 2, 3), 0, 1, 2, 3, List (4, List (5, List (6))))))
    println ("P07 solution given list = List () is:   " + flatten (List ()))
}