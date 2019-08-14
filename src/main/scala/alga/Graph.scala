package alga

// TODO: add GraphLike

/** '''Alga''' is a library for algebraic construction and manipulation of graphs.
 *  See [[https://github.com/snowleopard/alga-paper this paper]] for the
 *  motivation behind the library, the underlying theory, and a Haskell
 *  implementation.
 *
 *  The [[Graph]] trait is an immutable representation for graphs with vertices
 *  if type `A`. It comes with four graph construction primitives [[Empty]],
 *  [[Vertex]], [[Overlay]] and [[Connect]]. We also define operators [[+]] and
 *  [[*]] as convenient shortcuts for constructors [[Overlay]] and [[Connect]],
 *  respectively.
 *
 *  The equality ([[equals]]) satisfies the following axioms of algebraic graphs.
 *
 *  [[Overlay]] is commutative and associative:
 *  {{{x + y = y + x}}}
 *  {{{x + (y + z) = (x + y) + z}}}
 *
 *  [[Connect]] is associative and has [[Empty]] as the identity:
 *  {{{x * Empty = x}}}
 *  {{{Empty * x = x}}}
 *  {{{x * (y * z) = (x * y) * z}}}
 *
 *  [[Connect]] distributes over [[Overlay]]:
 *  {{{x * (y + z) == x * y + x * z}}}
 *  {{{(x + y) * z == x * z + y * z}}}
 *
 *  [[Connect]] can be decomposed:
 *  {{{x * y * z == x * y + x * z + y * z}}}
 *
 *  The following useful theorems can be proved from the above set of axioms.
 *
 *  [[Overlay]] has [[Empty]] as the identity and is idempotent:
 *  {{{x + Empty == x}}}
 *  {{{Empty + x == x}}}
 *  {{{x + x == x}}}
 *
 *  Absorption and saturation of [[Connect]]:
 *  {{{x * y + x + y == x * y}}}
 *  {{{x * x * x == x * x}}}
 *
 *  When specifying the time and memory complexity of graph algorithms, '''n'''
 *  will denote the number of vertices in the graph, '''m''' will denote the
 *  number of edges in the graph, and '''s''' will denote the size of the
 *  corresponding [[Graph]] expression. For example, if '''g''' is a [[Graph]]
 *  then '''n''', '''m''' and '''s''' can be computed as follows:
 *  {{{n == g.vertexCount}}}
 *  {{{m == g.edgeCount}}}
 *  {{{s == g.size}}}
 *
 *  @author  Andrey Mokhov
 *  @version 0.0.1
 */
sealed trait Graph[+A] {
    /** A convenient operator for overlaying graphs. Complexity: '''O(1)'''
    *   time and memory, '''O(s1 + s2)''' size.
    *   @param that the graph to overlay with this graph.
    *   @return     the resulting graph.
    *   {{{this + that == Overlay(this, that)}}}
    */
    def +[B >: A](that: Graph[B]): Graph[B] = Overlay(this, that)

    /** A convenient operator for connecting graphs. Complexity: '''O(1)'''
    *   time and memory, '''O(s1 + s2)''' size.
    *   @param that the graph to connect with this graph.
    *   @return     the resulting graph.
    *   {{{this * that == Connect(this, that)}}}
    */
    def *[B >: A](that: Graph[B]): Graph[B] = Connect(this, that)

    /** Comparison operator which establishes a partial order. Complexity: '''O(s + m * log(m))'''
     *   time and memory.
     *   @param that the graph to be compared with this graph.
     *   @return  `true` if this graph is greater than that, and false` otherwise.
     *   {{{this <= that == this isSubgraphOf that}}}
     */
    def <=[B >: A](that: Graph[B]): Boolean = this isSubgraphOf that

    /** Pretty-print the graph expression into a `String`. Complexity:
    *   '''O(s)''' time.
    *   @return the graph expression as a `String`.
    */
    override def toString = {
        def go(p: Boolean, graph: Graph[A]): String = graph match {
            case Empty              => "empty"
            case Vertex(x)          => x.toString
            case Overlay(x, y) if p => "(" + go(false, graph) + ")"
            case Overlay(x, y)      => go(false, x) + " + " + go(false, y)
            case Connect(x, y)      => go(true , x) + " * " + go(true , y)
        }
        go(false, this);
    }

    /** Generalised graph folding: recursively collapse the graph by applying
    *   the provided functions to the leaves and internal nodes of the expression.
    *   Complexity: '''O(s)''' applications of given functions. As an example, the
    *   complexity of the method [[size]] is '''O(s)''', since all functions
    *   have cost '''O(1)''':
    *   {{{def size: Int = foldg[Int](1, { _ => 1 }, _+_, _+_)}}}
    *   @param e the value to replace [[Empty]] leaves.
    *   @param v the function to apply to [[Vertex]] leaves.
    *   @param o the function to apply to [[Overlay]] nodes.
    *   @param c the function to apply to [[Connect]] nodes.
    *   @return  the result of graph folding.
    */
    def foldg[B](e: B, v: A => B, o: (B, B) => B, c: (B, B) => B): B =
        this match {
            case Empty         => e
            case Vertex(x)     => v(x)
            case Overlay(x, y) => o(x.foldg(e, v, o, c), y.foldg(e, v, o, c))
            case Connect(x, y) => c(x.foldg(e, v, o, c), y.foldg(e, v, o, c))
        }

    /** Check if the graph is empty. Complexity: '''O(s)''' time.
    *   @return `true` if the graph is empty, and `false` otherwise.
    */
    def isEmpty: Boolean = foldg[Boolean](true, { _ => false }, _&&_, _&&_)

    /** The size of the graph, that is the number of leaves of the expression
    *   including [[Empty]] leaves. Complexity: '''O(s)''' time.
    *   @return the size of the graph.
    */
    def size: Int = foldg[Int](1, { _ => 1 }, _+_, _+_)

    /** The number of vertices in the graph. Complexity: '''O(s * log(n))''' time.
    *   @return the number of vertices in the graph.
    */
    def vertexCount: Int = vertexSet.size

    /** The number of edges in the graph. Complexity: '''O(s + m * log(m))'''
        time. Note that the number of edges '''m''' of a graph can be quadratic
        with respect to the expression size '''s'''.
    *   @return the number of edges in the graph.
    */
    def edgeCount: Int = edgeSet.size

    /** Check if the graph contains a given vertex. Complexity: '''O(s)''' time.
    *   @param v the vertex to check.
    *   @return  `true` if the graph contains `v`, and `false` otherwise.
    */
    def hasVertex[B >: A](v: B): Boolean = foldg[Boolean](false, _ == v, _||_, _||_)

    /** Check if the graph contains a given edge. Complexity: '''O(s)''' time.
    *   @param u the source vertex of the edge.
    *   @param v the target vertex of the edge.
    *   @return  `true` if the graph contains the edge from `u` to `v`, and
                 `false` otherwise.
    */
    def hasEdge[B >: A](u: B, v: B): Boolean =
        Graph.edge(u, v).isSubgraphOf(induce { w => w == u || w == v })

    /** Transform the graph by applying a function to each of its vertices.
    *   Complexity: '''O(s)''' time.
    *   @param f the function to apply to the graph vertices.
    *   @return  the resulting graph.
    */
    def map[B](f: A => B): Graph[B] =
        foldg(Empty, f.andThen(Vertex[B]), Overlay[B], Connect[B])

    /** Transform the graph by substituting each of its vertices with a subgraph.
    *   Complexity: '''O(s)''' time.
    *   @param f the function computing the subgraphs to substitute the graph
    *            vertices.
    *   @return  the resulting graph.
    */
    def flatMap[B](f: A => Graph[B]): Graph[B] =
        foldg(Empty, f, Overlay[B], Connect[B])

    /** Construct the ''induced subgraph'' of the graph by removing the vertices
    *   that do not satisfy a given predicate. Complexity: '''O(s)''' time,
    *   memory and size, assuming that the predicate takes '''O(1)''' to be
    *   evaluated.
    *   @param p the predicate selecting the vertices for the induced subgraph.
    *   @return  the induced subgraph on `p`.
    */
    def induce(p: A => Boolean): Graph[A] =
        flatMap { x => if (p(x)) Vertex(x) else Empty }

    /** Remove a vertex from the graph. Complexity: '''O(s)''' time, memory and
    *   size.
    *   @param v the vertex to remove.
    *   @return  the resulting graph.
    */
    def removeVertex[B >: A](v: B): Graph[A] = induce(_ != v)

    /** The method `replaceVertex u v` replaces the vertex `u` with the vertex
    *   `v` in the graph. If `v` already exists, `u` and `v` will be merged.
    *   Complexity: '''O(s)''' time, memory and size.
    *   @param u the vertex to be replaced.
    *   @param v the replacement vertex.
    *   @return  the resulting graph.
    */
    def replaceVertex[B >: A](u: B, v: B): Graph[B] =
        map { w => if (u == w) v else u }

    /** Merge vertices satisfying a given predicate into a given vertex.
    *   Complexity: '''O(s)''' time, memory and size, assuming that the predicate
    *   takes 'O(1)' to be evaluated.
    *   @param p the predicate selecting the vertices to be merged.
    *   @param v the target vertex.
    *   @return  the resulting graph.
    */
    def mergeVertices[B >: A](p: A => Boolean, v: B): Graph[B] =
        map { u => if (p(u)) v else u }

    /** Split a vertex into a list of vertices with the same connectivity.
    *   Complexity: '''O(s + k * L)''' time, memory and size, where '''k''' is
    *   the number of occurrences of the vertex in the expression and '''L''' is
    *   the length of the given list.
    *   @param u  the vertex to be split.
    *   @param vs the list of vertices that `u` will be split into.
    *   @return   the resulting graph.
    */
    def splitVertex[B >: A](u: B, vs: List[B]): Graph[B] =
        flatMap { w => if (u == w) Graph.vertices(vs) else Vertex(w) }

    /** The list of vertices of the graph, in the order they appear as leaves of
    *   the graph expression. Note that the list may contain repetitions.
    *   Complexity: '''O(s)''' time and '''O(n)''' memory.
    *   @return the list of vertices of the graph.
    */
    def toList: List[A] = foldg[List[A]](Nil, { x => List(x) }, _++_, _++_)

    /** The set of vertices of the graph. Complexity: '''O(s * log(n))''' time
    *   and '''O(n)''' memory.
    *   @return the set of vertices of the graph.
    */
    def vertexSet[B >: A]: Set[B] =
        foldg[Set[B]](Set.empty, { x => Set(x) }, _++_, _++_)

    /** The set of edges of the graph. Complexity: '''O(s * log(m))''' time and
    *   '''O(m)''' memory.
    *   @return the set of edges of the graph.
    */
    def edgeSet[B >: A]: Set[(B, B)] = this match {
        case Empty         => Set.empty
        case Vertex(x)     => Set.empty
        case Overlay(x, y) => x.edgeSet ++ y.edgeSet
        case Connect(x, y) => x.edgeSet ++ y.edgeSet ++
            x.vertexSet.flatMap { a: A => y.vertexSet.map { b: A => (a, b) } }
    }

    // TODO: Simplify or add a separate non-structural equality operator.
    /** Check if the graph is equal to a given graph. Complexity:
    *   '''O(s * log(m))''' time.
    *   @param that the given graph.
    *   @return     `true` if this graph is equal to `that` graph, and `false`
                    otherwise.
    */
    override def equals(that: Any): Boolean =
        if (this eq Empty) that match {
            case Vertex(_)     => false
            case Overlay(_, _) => false
            case Connect(_, _) => false
            case _: Graph[A]   => true  // Here that must be Empty.
            case _             => false
        }
        else that match {
            case that: Graph[A] => vertexSet == that.vertexSet && edgeSet == that.edgeSet
            case _              => false
        }

    /** Check if the graph is a subgraph of the given graph. Complexity:
    *   '''O(s + m * log(m))''' time. Note that the number of edges '''m''' of
    *   a graph can be quadratic with respect to the expression size '''s'''.
    *   @param that the given graph.
    *   @return     `true` if this graph is a subgraph of `that`, and `false`
    *               otherwise.
    */
    def isSubgraphOf[A](that: Graph[A]): Boolean = this + that == that
}

final case object Empty                                extends Graph[Nothing]
final case class  Vertex [A](a: A                    ) extends Graph[A]
final case class  Overlay[A](x: Graph[A], y: Graph[A]) extends Graph[A]
final case class  Connect[A](x: Graph[A], y: Graph[A]) extends Graph[A]

object Graph {
    /** Construct the ''empty'' graph. An alias for the constructor [[Empty]].
    *   Complexity: '''O(1)''' time, memory and size.
    *   @return the empty graph.
    */
    def empty: Graph[Nothing] = Empty

    /** Construct the graph comprising a ''single isolated vertex''. An alias
    *   for the constructor [[Vertex]]. Complexity: '''O(1)''' time, memory and
    *   size.
    *   @param v the vertex of type `A`.
    *   @return  the graph comprising a single vertex '''v'''.
    */
    def vertex[A](v: A): Graph[A] = Vertex(v)

    /** Construct the graph comprising a given list of isolated vertices.
    *   Complexity: '''O(L)''' time, memory and size, where '''L''' is the
    *   length of the given list.
    *   @param vs the list of vertices of type `A`.
    *   @return   the graph comprising isolated vertices from the list '''vs'''.
    */
    def vertices[A](vs: List[A]): Graph[A] = overlays(vs.map(vertex))

    /** Construct the graph comprising a ''single edge''. Complexity: '''O(1)'''
    *   time, memory and size.
    *   @param s the source vertex of type `A`.
    *   @param t the target vertex of type `A`.
    *   @return  the graph comprising a single edge connecting the source vertex
    *            '''s''' to the target vertex '''t'''.
    */
    def edge[A](s: A, t: A): Graph[A] = vertex(s) * vertex(t)

    /** Construct the graph from a list of edges. Complexity: '''O(L)''' time,
    *   memory and size, where '''L''' is the length of the given list.
    *   @param es the list of edges of type `(A, A)`.
    *   @return   the graph comprising edges from the list '''es'''.
    */
    def edges[A](es: List[(A, A)]): Graph[A] =
        overlays(es.map { case (u, v) => edge(u, v) })

    /** ''Overlay'' two graphs. An alias for the constructor [[Overlay]]. This is
    *   an idempotent, commutative and associative operation with the identity
    *   [[Empty]]. Complexity: '''O(1)''' time and memory, '''O(s1 + s2)''' size.
    *   @param x the first graph.
    *   @param y the second graph.
    *   @return  the overlay of the graphs '''x''' and '''y'''.
    */
    def overlay[A](x: Graph[A], y: Graph[A]): Graph[A] = x + y

    /** Overlay a given list of graphs. Complexity: '''O(L)''' time and memory,
    *   and '''O(S)''' size, where '''L''' is the length of the given list, and
    *   '''S''' is the sum of sizes of the graphs in the list.
    *   @param xs the list of graphs.
    *   @return   the overlay of graphs from the list '''xs'''.
    */
    def overlays[A](xs: List[Graph[A]]): Graph[A] =
        xs.foldRight(empty:Graph[A])(_+_)

    /** ''Connect'' two graphs. An alias for the constructor [[Connect]]. This
    *   is an associative operation with the identity [[Empty]], which
    *   distributes over overlay and obeys the decomposition axiom. Complexity:
    *   '''O(1)''' time and memory, '''O(s1 + s2)''' size. Note that the number
    *   of edges in the resulting graph is quadratic with respect to the number
    *   of vertices of the arguments: '''m = O(m1 + m2 + n1 * n2)'''.
    *   @param x the first graph.
    *   @param y the second graph.
    *   @return  the result of connecting the graph '''x''' to the graph '''y'''.
    */
    def connect[A](x: Graph[A], y: Graph[A]): Graph[A] = x * y

    /** Connect a given list of graphs. Complexity: '''O(L)''' time and memory,
    *   and '''O(S)''' size, where '''L''' is the length of the given list, and
    *   '''S''' is the sum of sizes of the graphs in the list.
    *   @param xs the list of graphs.
    *   @return   the result of connecting graphs from the list '''xs'''.
    */
    def connects[A](xs: List[Graph[A]]): Graph[A] =
        xs.foldRight(empty:Graph[A])(_*_)

    /** The ''clique'' on a list of vertices. Complexity: '''O(L)''' time, memory
    *   and size, where '''L''' is the length of the given list.
    *   @param vs the list of vertices of type `A`.
    *   @return   the graph corresponding to the clique on vertices `vs`.
    */
    def clique[A](xs: List[A]): Graph[A] = connects(xs.map(vertex))

    /** The ''biclique'' on two lists of vertices. Complexity: '''O(L1 + L2)'''
    *   time, memory and size, where '''L1''' and '''L2''' are the lengths of
    *   the given lists.
    *   @param us the first list of vertices of type `A`.
    *   @param vs the second list of vertices of type `A`.
    *   @return   the graph corresponding to the biclique on `us` and `vs`.
    */
    def biclique[A](us: List[A], vs: List[A]): Graph[A] =
        vertices(us) * vertices(vs)

    /** The ''star'' formed by a centre vertex and a list of leaves. Complexity:
    *   '''O(L)''' time, memory and size, where '''L''' is the length of the
    *   given list.
    *   @param u  the centre vertex of type `A`.
    *   @param vs the list of leaves of type `A`.
    *   @return   the graph corresponding to the star on `u` and `vs`.
    */
    def star[A](u: A, vs: List[A]): Graph[A] = vertex(u) * vertices(vs)

    /** The ''path'' on a list of vertices. Complexity: '''O(L)''' time, memory
    *   and size, where '''L''' is the length of the given list.
    *   @param vs the list of vertices of type `A`.
    *   @return   the graph corresponding to the path on vertices `vs`.
    */
    def path[A](vs: List[A]): Graph[A] = vs match {
        case Nil      => empty
        case v :: Nil => vertex(v)
        case vs       => edges(vs.zip(vs.tail))
    }

    /** The ''circuit'' on a list of vertices. Complexity: '''O(L)''' time, memory
    *   and size, where '''L''' is the length of the given list.
    *   @param vs the list of vertices of type `A`.
    *   @return   the graph corresponding to the circuit on vertices `vs`.
    */
    def circuit[A](vs: List[A]): Graph[A] = vs match {
        case Nil     => empty
        case v :: vs => path(List(v) ++ vs ++ List(v))
    }

    /** Compute the ''Cartesian product'' of graphs. Complexity: '''O(s1 * s2)'''
    *   time, memory and size, where '''s1''' and '''s2''' are the sizes of the
    *   given graphs.
    *   @param x the first graph.
    *   @param y the second graph.
    *   @return  the Cartesian product of `x` and `y`.
    */
    def box[A,B](x: Graph[A], y: Graph[B]): Graph[(A, B)] = {
        val xs = y.toList.map { b => x.map { a => (a, b) } }
        val ys = x.toList.map { a => y.map { b => (a, b) } }
        overlays(xs ++ ys)
    }

    /** Construct a ''mesh'' graph from two lists of vertices. Complexity:
    *   '''O(L1 * L2)''' time, memory and size, where 'L1' and 'L2' are the
    *   lengths of the given lists.
    *   @param us the first list of vertices of type `A`.
    *   @param vs the second list of vertices of type `A`.
    *   @return   the graph corresponding to the mesh on `us` and `vs`.
    */
    def mesh[A,B](us: List[A], vs: List[B]): Graph[(A, B)] =
        box(path(us), path(vs))

    /** Construct a ''torus'' graph from two lists of vertices. Complexity:
    *   '''O(L1 * L2)''' time, memory and size, where 'L1' and 'L2' are the
    *   lengths of the given lists.
    *   @param us the first list of vertices of type `A`.
    *   @param vs the second list of vertices of type `A`.
    *   @return   the graph corresponding to the torus on `us` and `vs`.
    */
    def torus[A,B](us: List[A], vs: List[B]): Graph[(A, B)] =
        box(circuit(us), circuit(vs))
}
