import scala.collection.mutable

case class Node[T](value: T, nodes: Seq[Node[T]])

object Node {


  def BFS[T](node: Node[T]) = {
    val queue = scala.collection.mutable.Queue[Node[T]]()
    queue.enqueue(node)

    def aux(node: List[List[Node[T]]], nextLayer: List[Node[T]]): List[Node[T]] = nextLayer match {
      case Nil => node.reverse.flatten
      case _ => aux(nextLayer :: node, nextLayer.map())
    }

    val size = queue.size
    while(!queue.isEmpty){
      println(queue.dequeue())
    }

    size
  }


  def DFS[T](node: Node[T]) = {
    def aux(node: Node[T], list: List[Node[T]]): List[Node[T]] = {
      println(node)
      node.nodes.flatMap(node => aux(node, node :: list)).toList
    }
    val list = aux(node, Nil)

  list.foreach(n => println(n))

  }


}