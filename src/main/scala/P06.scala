import scala.annotation.tailrec
import P05.reverse

// Problem 6
// (*) Find out whether a list is a palindrome.
//
//
// Example:
//
// scala> isPalindrome(List(1, 2, 3, 2, 1))
// res0: Boolean = true

object P06 extends App {
    def isPalindromeReverse (list : List[Any]) : Boolean = {
        list == reverse (list)
    }

    @tailrec
    def isPalindromeOriginal (list : List[Any]) : Boolean = {
        list match {
            case Nil => true
            case _ :: Nil => true
            case list => list.head == list.last && isPalindromeOriginal (list.slice (1, list.length - 1))
        }
    }

    @tailrec
    def isPalindrome (list : List[Any]) : Boolean = {
        list match {
            case Nil => true
            case _ :: Nil => true
            case list => list.head == list.last && isPalindrome (list.tail.init)
        }
    }

    println ("P06 solution using P05.reverse given list = List (0, 1, 2, 3, 4, 5)) is:   " + isPalindromeReverse (List (0, 1, 2, 3, 4, 5)))
    println ("P06 solution using P05.reverse given list = List (0, 1, 2, 3, 2, 1, 0)) is:   " + isPalindromeReverse (List (0, 1, 2, 3, 2, 1, 0)))

    println

    println ("P06 original solution given list = List (0, 1, 2, 3, 4, 5)) is:   " + isPalindromeOriginal (List (0, 1, 2, 3, 4, 5)))
    println ("P06 original solution given list = List (0, 1, 2, 3, 2, 1, 0)) is:   " + isPalindromeOriginal (List (0, 1, 2, 3, 2, 1, 0)))

    println

    println ("P06 solution given list = List (0, 1, 2, 3, 4, 5)) is:   " + isPalindrome (List (0, 1, 2, 3, 4, 5)))
    println ("P06 solution given list = List (0, 1, 2, 3, 2, 1, 0)) is:   " + isPalindrome (List (0, 1, 2, 3, 2, 1, 0)))

    println
}