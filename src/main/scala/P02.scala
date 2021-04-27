import scala.annotation.tailrec

// Problem 2
// (*) Find the last but one element of a list.
//
//
// Example:
//
// scala> penultimate(List(1, 1, 2, 3, 5, 8))
// res0: Int = 5

object P02 extends App {
    def penultimateInbuilt[A] (list : List[A]) : Option[A] = {
        if (list.length > 1) Some (list.init.last)
        else None
    }

    @tailrec
    def penultimate[A] (list : List[A]) : Option[A] = {
        list match {
            case Nil | _ :: Nil => None
            case hd :: _ :: Nil => Some (hd)
            case _ :: tl => penultimate (tl)
        }
    }

    println ("P02 solution using inbuilt collection API given list = List (0, 1, 2, 3, 4, 5) is:   " + P02.penultimateInbuilt (List (0, 1, 2, 3, 4, 5)))
    println ("P02 solution given list = List (0, 1, 2, 3, 4, 5) is:   " + P02.penultimate (List (0, 1, 2, 3, 4, 5)))
    println ("P02 solution using inbuilt collection API given list = List (0) is:   " + P02.penultimateInbuilt (List (0)))
    println ("P02 solution given list = List (0) is:   " + P02.penultimate (List (0)))
    println ("P02 solution using inbuilt collection API given list = List () is:   " + P02.penultimateInbuilt (List ()))
    println ("P02 solution given list = List () is:   " + P02.penultimate (List ()))
}