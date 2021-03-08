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
    def pack (list : List[Any]) : Option[List[List[Any]]] = {
        @tailrec
        def innerPack(packed : List[List[Any]], innerList : List[Any]) : Option[List[List[Any]]] = {
            innerList match {
                case Nil => Some (packed)
                case hd :: tl if packed.nonEmpty && hd == packed.last.head => innerPack (packed.init ::: List (packed.last ::: List (hd)), tl)
                case hd :: tl => innerPack (packed ::: List (List (hd)), tl)
            }
        }
        innerPack (List (), list)
    }

    def encode (list : List[List[Any]]) : Option[List[(Int, Any)]] = {
        @tailrec
        def innerEncode (encoded : List[(Int, Any)], innerList : List[List[Any]]) : Option[List[(Int, Any)]] = {
            innerList match {
                case Nil => Some (encoded)
                case hd :: tl => innerEncode (encoded ::: List ((hd.length, hd.head)), tl)
            }
        }
        innerEncode (List (), list)
    }
    // until here

    def encodeDirect (list : List[Any]) : Option[List[(Int, Any)]] = {
        @tailrec
        def innerEncodeDirect (encoded : List[(Int, Any)], innerList : List[Any]) : Option[List[(Int, Any)]] = {
            innerList match {
                case Nil => Some (encoded)
                case hd :: tl if encoded.nonEmpty && encoded.last._2 == hd => innerEncodeDirect (encoded.init ::: List ((encoded.last._1 + 1, hd)), tl)
                case hd :: tl => innerEncodeDirect (encoded ::: List ((1, hd)), tl)
            }
        }
        innerEncodeDirect (List (), list)
    }

    println ("P13 -indirect- solution given List ('a', 'a', 'b', 'c', 'c', 'b') is:   " + encode (pack (List ('a', 'a', 'b', 'c', 'c', 'b')) get))
    println ("P13 -indirect- solution given List () is:   " + encode (pack (List ()) get))
    println ("P13 -direct- solution given List ('a', 'a', 'b', 'c', 'c', 'b') is:   " + encodeDirect (List ('a', 'a', 'b', 'c', 'c', 'b')))
    println ("P13 -direct- solution given List () is:   " + encodeDirect (List ()))
}