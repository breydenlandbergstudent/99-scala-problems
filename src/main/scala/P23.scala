import P20.remove
import scala.util.Random
import scala.annotation.tailrec

// Problem 23
// (**) Extract a given number of randomly selected elements from a list.
//
//
// Example:
//
// scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
// res0: List[Symbol] = List('e, 'd, 'a)
//
// Hint: Use the solution to problem P20

// not finished
// TODO
object P23 extends App {
    val random : Random = scala.util.Random
    def randomlySelect (N : Int, list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerRandomlySelect (randomList : List[Any], innerN : Int, innerList : List[Any]) : Option[List[Any]] = {
            val r = random.nextInt (list.length - 1)
            innerN match {
                case 0 => Some (randomList)
                case _ =>
                    val s = List (innerList (r))
                    val t = remove (r, innerList)
                    innerRandomlySelect (randomList ::: s, innerN - 1, t.get._1)
            }
        }
        innerRandomlySelect (List (), N, list)
    }

    println ("P23 solution given N = 3, list = List (0, 1, 2, 3, 4, 5)) is:   " + randomlySelect (3, List (0, 1, 2, 3, 4, 5)))
    println ("P23 solution given N = 3, list = List (0, 1, 2, 3, 4, 5)) is:   " + randomlySelect (3, List ()))
}