package alga

sealed trait AdjacencyMap[+A] {

}

final case class AdjacencyMap[A](adjacencyMap: Map[A, Set[A]]) extends AdjacencyMap[A]

object AdjacencyMap {
  /** Construct the ''empty'' adjacency map.
   *   Complexity: '''O(1)''' time, memory and size.
   *   @return the empty adjacency map.
   */
  def empty: AdjacencyMap[Nothing] = AdjacencyMap(Map.empty)

  /** Construct the adjacency map comprising a ''single isolated vertex''. An alias
   *   for the constructor [[Vertex]]. Complexity: '''O(1)''' time, memory and
   *   size.
   *   @param v the vertex of type `A`.
   *   @return  the adjacency map comprising a single vertex '''v'''.
   */
  def vertex[A](a: A) =  AdjacencyMap(Map(a -> Set.empty))

  /** Construct the graph comprising a ''single isolated vertex''. An alias
   *   for the constructor [[Vertex]]. Complexity: '''O(1)''' time, memory and
   *   size.
   *   @param v the vertex of type `A`.
   *   @return  the graph comprising a single vertex '''v'''.
   */
  /**
   * Construct the adjacency map comprising of a ''single edge''
   * Complexity: '''O(1)''' time, memory and
   * *   size.
   */
  def edge[A](x: A, y: A) =
    if (x == y) AdjacencyMap(Map(x -> Set(y)))
    else AdjacencyMap(Map(x -> Set(y), y -> Set.empty))
}