import scala.collection.mutable

case class Node[T](value: T, nodes: Seq[Node[T]])

object Node {


  def BFS[T](node: Node[T]) = {
    // color 0 for white, 1 for grey and 2 for black
    val map = scala.collection.mutable.HashMap[Node[T], Int]()
    val queue = scala.collection.mutable.Queue[Node[T]]()
    val finalQueue = scala.collection.mutable.Queue[Node[T]]()
    queue.enqueue(node)

    def fillColor(node: Node[T]): Unit = {
      map.put(node, (0))
      node.nodes.map(fillColor)
    }

    def getColor(node: Node[T]) = map.getOrElse(node, 0)

    def aux(queue: mutable.Queue[Node[T]]): Unit = {
      if (queue.isEmpty) return
      else {
        val node = queue.dequeue()
        //white
        if (getColor(node) == 0) {
          map.update(node, (1))
          queue.enqueue(node)
          aux(queue)
        }
        //grey
        else if (getColor(node) == 1) {
          map.update(node, (2))
          queue.enqueue(node)
          aux(queue)
        }
        // black
        else {
          node.nodes.foreach(queue.enqueue(_))
          finalQueue.enqueue(node)
          aux(queue)
        }
      }
    }

    def displayQueue = finalQueue.map(n => println(n.value))

    fillColor(node)
    aux(queue)
    finalQueue.map(_.value)
  }


  def DFS[T](node: Node[T]) = {

    val stack = scala.collection.mutable.Stack[Node[T]]()

    def aux(node: Node[T]): Unit = {
      stack.push(node)
      node.nodes.map(n => aux(n))
    }
    aux(node)
    stack.map(_.value).reverse

  }


}