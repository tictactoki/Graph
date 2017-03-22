/**
  * Created by stephane on 22/03/2017.
  */
sealed trait BinaryTree {
  val value: Int
}

case class Node(override val value: Int, left: BinaryTree, right: BinaryTree) extends BinaryTree
case class Leaf(override val value: Int) extends BinaryTree


object BinaryTree {

  val b = Node(1, Node(2, Leaf(4), Leaf(5)), Node(6, Leaf(8), Node(9, null, Leaf(10))))

  /**
    * Display when we are at left of node
    *
    * @param binaryTree
    */
  def prefixTraverser(binaryTree: BinaryTree): Unit = binaryTree match {
    case Node(value, left, right) =>
      println("Node:" + value)
      if (left != null)
        prefixTraverser(left)
      if (right != null)
        prefixTraverser(right)
    case Leaf(value) => println("Leaf: " + value)
  }

  /**
    * Display when we are at right of node
    *
    * @param binaryTree
    */
  def postfixTraverser(binaryTree: BinaryTree): Unit = binaryTree match {
    case Node(value, left, right) =>
      if (left != null)
        postfixTraverser(left)
      if (right != null)
        postfixTraverser(right)
      println("Node:" + value)
    case Leaf(value) => println("Leaf: " + value)
  }

  /**
    * Display when we are at bottom of node
    *
    * @param binaryTree
    */
  def infixTraverser(binaryTree: BinaryTree): Unit = binaryTree match {
    case Node(value, left, right) =>
      if (left != null)
        infixTraverser(left)
      println("Node:" + value)
      if (right != null)
        infixTraverser(right)
    case Leaf(value) => println("Leaf: " + value)
  }

  def binarySearch(binaryTree: BinaryTree, value: Int, f: (Int, Int) => Boolean): Boolean = {
    if (binaryTree == null) false
    else binaryTree match {
      case node: Node =>
        if (f(node.value, value)) true
        else {
          binarySearch(node.left, value, f) || binarySearch(node.right, value, f)
        }
      case Leaf(v) =>
        v == value
    }
  }


  def getValue(binaryTree: BinaryTree, value: Int, f: (Int, Int) => Int): Int = {
    if (binaryTree == null) value
    else binaryTree match {
      case node: Node =>
        f(getValue(node.left, f(value, node.value), f), getValue(node.right, f(value, node.value), f))
      case Leaf(v) =>
        f(value, v)
    }
  }

  def isBinaryTree(binaryTree: BinaryTree): Boolean = {
    if (binaryTree == null) true
    else binaryTree match {
      case Node(v,left,right) =>
        if(left != null) {
          if(left.value > v) return false
          else isBinaryTree(left)
        }
        if(right != null) {
          if(right.value < v) return false
          else isBinaryTree(right)
        }
        return true
      case Leaf(v) => true
    }
  }


  /*def insert(binaryTree: BinaryTree, value: Int): BinaryTree = {

    def insert(root: BinaryTree, tree: BinaryTree, value: Int): BinaryTree = {
      binaryTree match {
        case Node(v,left,right) =>
          if(value < v) {
            val r = if(root == null) Node(v,Leaf(left.value),right) else {

              root
            }
            insert(r,left,value)
          }
          else if(value > v) {
            val r = if(root == null) Node(v,left,Leaf(right.value)) else root
            insert(r,right,value)
          }
          else root
        case Leaf(v) =>
          if(value < v) {
            val r = if(root == null) Node(v, Leaf(value), null) else {
              Node(v, Leaf(value), null)
            }
          }
      }
    }

    insert(null,binaryTree,value)

  }*/


}