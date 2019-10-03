package alga
import scalaz._, Scalaz._

final case class AdjacencyMap[A](adjacencyMap: Map[A, Set[A]])

object AdjacencyMap {
  /** Construct the ''empty'' adjacency map.
   *   Complexity: '''O(1)''' time, memory and size.
   *   @return the empty adjacency map.
   */
  def empty[A]: AdjacencyMap[A] = AdjacencyMap(Map())

  /** Construct the adjacency map comprising a ''single isolated vertex''. An alias
   *   for the constructor [[Vertex]]. Complexity: '''O(1)''' time, memory and
   *   size.
   *   @param v the vertex of type `A`.
   *   @return  the adjacency map comprising a single vertex '''v'''.
   */
  def vertex[A](a: A): AdjacencyMap[A] =  AdjacencyMap(Map(a -> Set.empty))

  /**
   * Construct the adjacency map comprising of a ''single edge''
   * Complexity: '''O(1)''' time, memory and
   * *   size.
   */
  def edge[A](x: A, y: A): AdjacencyMap[A] =
    if (x == y) AdjacencyMap(Map(x -> Set(y)))
    else AdjacencyMap(Map(x -> Set(y), y -> Set[A]()))

  def overlay[A](x: AdjacencyMap[A], y: AdjacencyMap[A]): AdjacencyMap[A] =
    (x, y) match {
      case (AdjacencyMap(x), AdjacencyMap(y)) => AdjacencyMap[A](x |+| y)
    }
}