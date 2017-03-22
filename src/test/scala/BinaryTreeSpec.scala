import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by stephane on 22/03/2017.
  */
class BinaryTreeSpec extends FlatSpec with Matchers{

  val binary = Node(6, Node(3, Leaf(2), Leaf(4)), Node(15, Leaf(8), Node(20, null, Leaf(21))))
  val tree = Node(1, Node(2, Leaf(4), Leaf(5)), Node(6, Leaf(8), Node(9, null, Leaf(10))))

  val eq = (a:Int,b:Int) => a == b
  val lt = (a:Int, b:Int) => if(a < b) a else b
  val gt = (a:Int, b:Int) => if(a > b) a else b

  "A tree search function" should "find the value if it exist" in {
    BinaryTree.binarySearch(binary, 8, eq) should be (true)
  }

  "A tree search function" should "return false if the value doesn't exist" in {
    BinaryTree.binarySearch(binary, 11, eq) should be (false)
  }

  "A tree getValue function" should "return the min value of the tree" in {
    BinaryTree.getValue(binary, Integer.MAX_VALUE, lt) shouldEqual(2)
  }

  "A tree getValue function" should "return the max value of the tree" in {
    BinaryTree.getValue(binary, Integer.MIN_VALUE, gt) shouldEqual(21)
  }

  "A binary tree function" should "return true if it's a binary tree" in {
    BinaryTree.isBinaryTree(binary) should be(true)
  }

  "A binary tree function" should "return false if it's a normal tree" in {
    BinaryTree.isBinaryTree(tree) should be(false)
  }


}
