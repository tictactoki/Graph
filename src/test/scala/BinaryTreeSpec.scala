import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by stephane on 22/03/2017.
  */
class BinaryTreeSpec extends FlatSpec with Matchers{


  val binary = SimpleNode(6, SimpleNode(3, SimpleLeaf(2), SimpleLeaf(4)), SimpleNode(15, SimpleLeaf(8), SimpleNode(20, null, SimpleLeaf(21))))
  val node = SimpleNode(1, null,null)
  val tree = SimpleNode(1, SimpleNode(2, SimpleLeaf(4), SimpleLeaf(5)), SimpleNode(6, SimpleLeaf(8), SimpleNode(9, null, SimpleLeaf(10))))

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
    BinaryTree.isBinaryTree(node) should be(true)
  }

  "A binary tree function" should "return false if it's a normal tree" in {
    BinaryTree.isBinaryTree(tree) should be(false)
  }


}
