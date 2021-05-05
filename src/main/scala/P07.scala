// Problem 7
// (**) Flatten a nested list structure.
//
//
// Example:
//
// scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
// res0: List[Any] = List(1, 1, 2, 3, 5, 8)

object P07 extends App {
    def flatten[A] (list : List[A]) : List[A] = {
        list match {
            case Nil => Nil
            case (hd : List[A]) :: tl => flatten (hd) ::: flatten (tl)
            case (hd : A) :: tl => hd :: flatten (tl)
        }
    }

    println ("P07 solution given list = List (List (0, 1, 2, 3), 0, 1, 2, 3, List (4, List (5, List (6)))) is:   " + flatten (List (List (0, 1, 2, 3), 0, 1, 2, 3, List (4, List (5, List (6))))))
    println ("P07 solution given list = List () is:   " + flatten (List ()))

    println
}