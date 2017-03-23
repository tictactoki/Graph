/**
  * Created by stephane on 22/03/2017.
  */
sealed trait BinaryTree {
  val value: Int
}

case class SimpleNode(override val value: Int, left: BinaryTree, right: BinaryTree) extends BinaryTree
case class SimpleLeaf(override val value: Int) extends BinaryTree

sealed trait CompleteBinaryTree {
  val value: Int
  val father: CompleteBinaryTree
}

case class CompleteNode(override val value: Int, override val father: CompleteNode = null, left: BinaryTree = null, right: BinaryTree = null) extends CompleteBinaryTree
case class CompleteLeaf(override val value: Int, override val father: CompleteNode) extends CompleteBinaryTree


object BinaryTree {

  val b = SimpleNode(1, SimpleNode(2, SimpleLeaf(4), SimpleLeaf(5)), SimpleNode(6, SimpleLeaf(8), SimpleNode(9, null, SimpleLeaf(10))))

  /**
    * Display when we are at left of SimpleNode
    *
    * @param binaryTree
    */
  def prefixTraverser(binaryTree: BinaryTree): Unit = binaryTree match {
    case SimpleNode(value, left, right) =>
      println("SimpleNode:" + value)
      if (left != null)
        prefixTraverser(left)
      if (right != null)
        prefixTraverser(right)
    case SimpleLeaf(value) => println("Leaf: " + value)
  }

  /**
    * Display when we are at right of SimpleNode
    *
    * @param binaryTree
    */
  def postfixTraverser(binaryTree: BinaryTree): Unit = binaryTree match {
    case SimpleNode(value, left, right) =>
      if (left != null)
        postfixTraverser(left)
      if (right != null)
        postfixTraverser(right)
      println("SimpleNode:" + value)
    case SimpleLeaf(value) => println("Leaf: " + value)
  }

  /**
    * Display when we are at bottom of SimpleNode
    *
    * @param binaryTree
    */
  def infixTraverser(binaryTree: BinaryTree): Unit = binaryTree match {
    case SimpleNode(value, left, right) =>
      if (left != null)
        infixTraverser(left)
      println("SimpleNode:" + value)
      if (right != null)
        infixTraverser(right)
    case SimpleLeaf(value) => println("Leaf: " + value)
  }

  def binarySearch(binaryTree: BinaryTree, value: Int, f: (Int, Int) => Boolean): Boolean = {
    if (binaryTree == null) false
    else binaryTree match {
      case node: SimpleNode =>
        if (f(node.value, value)) true
        else {
          binarySearch(node.left, value, f) || binarySearch(node.right, value, f)
        }
      case SimpleLeaf(v) =>
        v == value
    }
  }


  def getValue(binaryTree: BinaryTree, value: Int, f: (Int, Int) => Int): Int = {
    if (binaryTree == null) value
    else binaryTree match {
      case node: SimpleNode =>
        f(getValue(node.left, f(value, node.value), f), getValue(node.right, f(value, node.value), f))
      case SimpleLeaf(v) =>
        f(value, v)
    }
  }

  def isBinaryTree(binaryTree: BinaryTree): Boolean = {
    if (binaryTree == null) true
    else binaryTree match {
      case SimpleNode(v,left,right) =>
        if(left != null) {
          if(left.value > v) return false
          else isBinaryTree(left)
        }
        if(right != null) {
          if(right.value < v) return false
          else isBinaryTree(right)
        }
        return true
      case SimpleLeaf(v) => true
    }
  }



  /*def insert(completeBinaryTree: CompleteBinaryTree, value: Int): CompleteBinaryTree = {

    def insert(root: CompleteBinaryTree, tree: CompleteBinaryTree, value: Int) = {
      if(root == null && tree == null) return CompleteNode(value)
      tree match {
        case CompleteNode(v,father,left,right) =>
          if(value < v) {
            val r =
          }
      }
    }

  }*/


}