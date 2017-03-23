import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by wong on 23/03/17.
  */
class GraphSpec extends FlatSpec with Matchers {

  type T = Int

  val nodes = Node(1, List(Node(2, List(Node(3, List(Node(4, Nil))), Node(5, Nil), Node(6, List(Node(7, Nil), Node(8, Nil), Node(9, List(Node(10, Nil), Node(11, List(Node(12, Nil))))))))), Node(13, Nil), Node(14, List(Node(15, Nil)))))

  "A BFS" should "display the right value on bfs order" in {
    Node.BFS(nodes) should equal(15)
  }

  "A DFS" should "display the rigth value on the dfs order" in {
    Node.DFS(nodes)
  }

}
