import org.scalatest._
import scala99._

class scala99Spec extends FlatSpec with Matchers {
  "Hello" should "have tests" in {
    true should === (true)
  }

  "P01" should "Find the last element of a list." in {
    last(List(1, 1, 2, 3, 5, 8)) shouldEqual 8
  }

  "P02" should "Find the last but one element of a list." in {
    penultimate(List(1, 1, 2, 3, 5, 8)) shouldEqual 5
  }

  "P03" should "Find the Kth element of a list" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) shouldEqual 2
  }

  "P04" should "Find the number of elements of a list" in {
    scala99.length(List(1, 1, 2, 3, 5, 8)) shouldEqual 6
  }

  "P05" should "Reverse a list." in {
    reverse(List(1, 1, 2, 3, 5, 8)) shouldBe List(8, 5, 3, 2, 1, 1)
  }

  "P06" should "Find out whether a list is a palindrome." in {
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
  }

  "P07" should "Flatten a nested list structure." in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
  }

  "P08" should "Eliminate consecutive duplicates of list elements." in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual
      List('a, 'b, 'c, 'a, 'd, 'e)
  }

  "P09" should " Pack consecutive duplicates of list elements into sublists." in {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  "P10" should "Run-length encoding of a list." in {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe
      List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }

  "P11" should "Modified run-length encoding." in {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe
       List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  }

  "P12" should "Decode a run-length encoded list." in {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) shouldEqual
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  "P13" should "Run-length encoding of a list (direct solution)." in {
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe
      List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }

  "P14" should "Duplicate the elements of a list." in {
    duplicate(List('a, 'b, 'c, 'c, 'd)) shouldEqual List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }

  "P15" should "Duplicate the elements of a list a given number of times." in {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) shouldEqual List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  "P16" should "Drop every Nth element from a list." in {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "P17" should "Split a list into two parts." in {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  "P18" should "Extract a slice from a list." in {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('d, 'e, 'f, 'g)
  }

  "P19" should "Rotate a list N places to the left." in {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }

  "P20" should "Remove the Kth element from a list." in {
    removeAt(1, List('a, 'b, 'c, 'd)) shouldEqual (List('a, 'c, 'd),'b)
  }

  "P21" should " Insert an element at a given position into a list." in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) shouldEqual List('a, 'new, 'b, 'c, 'd)
  }

  "P22" should "Create a list containing all integers within a given range." in {
    range(4, 9) shouldBe List(4, 5, 6, 7, 8, 9)
  }

  "P23" should "Extract a given number of randomly selected elements from a list." in {
    val sample = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    val res = randomSelect(3, sample)
    res.length shouldEqual 3
  }

  "P24" should " Lotto: Draw N different random numbers from the set 1..M." in {
    lotto(6, 49).length shouldEqual 6
  }

  "P25" should "Generate a random permutation of the elements of a list." in {
    val sample = List('a', 'b', 'c', 'd', 'e', 'f')
    val res = randomPermute(sample)
    res.sorted shouldEqual sample.sorted
  }

  "P26" should "Generate the combinations of K distinct objects chosen from the N elements of a list." in {
    combinations(3, (1 to 12).toList).length shouldEqual 220
  }
}
