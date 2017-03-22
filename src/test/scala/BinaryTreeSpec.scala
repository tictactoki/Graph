import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by stephane on 22/03/2017.
  */
class BinaryTreeSpec extends FlatSpec with Matchers{

  val binary = BinaryTree.b

  val eq = (a:Int,b:Int) => a == b
  val lt = (a:Int, b:Int) => if(a < b) a else b
  val gt = (a:Int, b:Int) => if(a > b) a else b

  "A binary search function" should "find the value if it exist" in {
    BinaryTree.binarySearch(binary, 8, eq) should be (true)
  }

  "A binary search function" should "return false if the value doesn't exist" in {
    BinaryTree.binarySearch(binary, 11, eq) should be (false)
  }

  "A binary tree getValue" should "return the min value of the tree" in {
    BinaryTree.getValue(binary, Integer.MAX_VALUE, lt) shouldEqual(1)
  }

  "A binary tree getValue" should "return the max value of the tree" in {
    BinaryTree.getValue(binary, Integer.MIN_VALUE, gt) shouldEqual(10)
  }

}
