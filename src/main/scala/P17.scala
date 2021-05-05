import scala.annotation.tailrec

// Problem 17
// (*) Split a list into two parts.
// The length of the first part is given. Use a Tuple for your result.
//
//
// Example:
//
// scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

object P17 extends App {
    def splitInbuilt[A] (K : Int, list : List[A]) : Option[(List[A], List[A])] = {
        Some (list splitAt K)
    }

    def splitOriginal[_] (K : Int, list : List[_]) : Option[(List[_], List[_])] = {
        @tailrec
        def innerSplit[A] (split : (List[A], List[A]), innerK : Int, innerList : List[A]) : Option[(List[A], List[A])] = {
            innerList match {
                case Nil => Some (split)
                case hd :: tl if innerK > 0 => innerSplit ((split._1 :+ hd, split._2), innerK - 1, tl)
                case hd :: tl => innerSplit ((split._1, split._2 :+ hd), innerK, tl)
            }
        }
        innerSplit ((List (), List ()), K, list)
    }

    // more idiomatic than original solution above
    def split[_] (K : Int, list : List[_]) : Option[(List[_], List[_])] = {
        @tailrec
        def innerSplit[A] (split : (List[A], List[A]), innerList : List[A]) : Option[(List[A], List[A])] = {
            innerList match {
                case Nil => Some (split)
                case hd :: tl if list.length - innerList.length < K => innerSplit ((split._1 :+ hd, split._2), tl)
                case hd :: tl => innerSplit ((split._1, split._2 :+ hd), tl)
            }
        }
        innerSplit ((List (), List ()), list)
    }

    println ("P17 solution using inbuilt collection API method given K = 3, list = List (0, 1, 2, 3, 4, 5) is:   " + splitInbuilt (3, List (0, 1, 2, 3, 4, 5)))
    println ("P17 solution using inbuilt collection API method given K = 3, list = List () is:   " + splitInbuilt (3, List ()))

    println

    println ("P17 original solution given K = 3, list = List (0, 1, 2, 3, 4, 5) is:   " + splitOriginal (3, List (0, 1, 2, 3, 4, 5)))
    println ("P17 original solution given K = 3, list = List () is:   " + splitOriginal (3, List ()))

    println

    println ("P17 solution given K = 3, list = List (0, 1, 2, 3, 4, 5) is:   " + split (3, List (0, 1, 2, 3, 4, 5)))
    println ("P17 solution given K = 3, list = List (0, 1, 2, 3, 4, 5, 6, 7) is:   " + split (3, List (0, 1, 2, 3, 4, 5, 6, 7)))
    println ("P17 solution given K = 5, list = List (0, 1, 2, 3, 4, 5, 6, 7) is:   " + split (5, List (0, 1, 2, 3, 4, 5, 6, 7)))
    println ("P17 solution given K = 3, list = List () is:   " + split (3, List ()))

    println
}