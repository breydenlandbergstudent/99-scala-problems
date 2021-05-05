import scala.annotation.tailrec

// Problem 13
// (**) Run-length encoding of a list (direct solution).
// Implement the so-called run-length encoding data compression method directly.
// I.e. don't use other methods you've written (like P09's pack); do all the work directly.
//
//
// Example:
//
// scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

object P13 extends App {
    // from here, just rewriting of pack and encode from memory - not the "direct" solution
    def pack[_] (list : List[_]) : Option[List[List[_]]] = {
        @tailrec
        def innerPack[A] (packed : List[List[A]], innerList : List[A]) : Option[List[List[Any]]] = {
            innerList match {
                case Nil => Some (packed)
                case hd :: tl if packed.nonEmpty && hd == packed.last.head => innerPack (packed.init :+ (packed.last :+ hd), tl)
                case hd :: tl => innerPack (packed :+ List (hd), tl)
            }
        }
        innerPack (List (), list)
    }

    def encode[_] (list : List[List[_]]) : Option[List[(Int, _)]] = {
        @tailrec
        def innerEncode[A] (encoded : List[(Int, A)], innerList : List[List[A]]) : Option[List[(Int, A)]] = {
            innerList match {
                case Nil => Some (encoded)
                case hd :: tl => innerEncode (encoded :+ (hd.length, hd.head), tl)
            }
        }
        innerEncode (List (), list)
    }
    // until here

    def encodeDirect[_] (list : List[_]) : Option[List[(Int, _)]] = {
        @tailrec
        def innerEncodeDirect[A] (encoded : List[(Int, A)], innerList : List[A]) : Option[List[(Int, A)]] = {
            innerList match {
                case Nil => Some (encoded)
                case hd :: tl if encoded.nonEmpty && encoded.last._2 == hd => innerEncodeDirect (encoded.init :+ (encoded.last._1 + 1, hd), tl)
                case hd :: tl => innerEncodeDirect (encoded :+ (1, hd), tl)
            }
        }
        innerEncodeDirect (List (), list)
    }

    println ("P13 indirect solution given List ('a', 'a', 'b', 'c', 'c', 'b') is:   " + encode (pack (List ('a', 'a', 'b', 'c', 'c', 'b')) get))
    println ("P13 indirect solution given List () is:   " + encode (pack (List ()) get))

    println

    println ("P13 solution given List ('a', 'a', 'b', 'c', 'c', 'b') is:   " + encodeDirect (List ('a', 'a', 'b', 'c', 'c', 'b')))
    println ("P13 solution given List () is:   " + encodeDirect (List ()))

    println
}