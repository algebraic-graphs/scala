// package alga
// TODO: add GraphLike
sealed trait Graph[+A] {
    def +[B >: A](that: Graph[B]): Graph[B] = Overlay(this, that)
    def *[B >: A](that: Graph[B]): Graph[B] = Connect(this, that)

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

    def isEmpty: Boolean = this match {
        case Empty         => true
        case Vertex(x)     => false
        case Overlay(x, y) => x.isEmpty && y.isEmpty
        case Connect(x, y) => x.isEmpty && y.isEmpty
    }

    def map[B](f: A => B): Graph[B] = this match {
        case Empty         => Empty
        case Vertex(x)     => Vertex(f(x))
        case Overlay(x, y) => x.map(f) + y.map(f)
        case Connect(x, y) => x.map(f) * y.map(f)
    }

    def flatMap[B](f: A => Graph[B]): Graph[B] = this match {
        case Empty         => Empty
        case Vertex(x)     => f(x)
        case Overlay(x, y) => x.flatMap(f) + y.flatMap(f)
        case Connect(x, y) => x.flatMap(f) * y.flatMap(f)
    }

    def induce(p: A => Boolean): Graph[A] =
        flatMap { x => if (p(x)) Vertex(x) else Empty }

    def removeVertex[B >: A](x: B): Graph[A] = induce(_ != x)

    def replaceVertex[B >: A](x: B, y: B): Graph[B] =
        map { z => if (x == z) y else x }

    def mergeVertices[B >: A](p: A => Boolean, x: B): Graph[B] =
        map { y => if (p(y)) x else y }

    def splitVertex[B >: A](x: B, ys: List[B]): Graph[B] =
        flatMap { v => if (x == v) Graph.vertices(ys) else Vertex(v) }

    def toList: List[A] = this match {
        case Empty         => Nil
        case Vertex(x)     => List(x)
        case Overlay(x, y) => x.toList ++ y.toList
        case Connect(x, y) => x.toList ++ y.toList
    }

    def vertexSet[B >: A]: Set[B] = this match {
        case Empty         => Set.empty
        case Vertex(x)     => Set(x)
        case Overlay(x, y) => x.vertexSet ++ y.vertexSet
        case Connect(x, y) => x.vertexSet ++ y.vertexSet
    }

    def edgeSet[B >: A]: Set[(B, B)] = this match {
        case Empty         => Set.empty
        case Vertex(x)     => Set.empty
        case Overlay(x, y) => x.edgeSet ++ y.edgeSet
        case Connect(x, y) => x.edgeSet ++ y.edgeSet ++
            x.vertexSet.flatMap { a: A => y.vertexSet.map { b: A => (a, b) } }
    }

    // TODO: Simplify or replace with a separate non-structural equality operator.
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

    def isSubgraphOf[A](x: Graph[A]): Boolean = this + x == x
}

final case object Empty                                extends Graph[Nothing]
final case class  Vertex [A](a: A                    ) extends Graph[A]
final case class  Overlay[A](x: Graph[A], y: Graph[A]) extends Graph[A]
final case class  Connect[A](x: Graph[A], y: Graph[A]) extends Graph[A]

object Graph {
    def empty: Graph[Nothing] = Empty

    def vertex[A](x: A): Graph[A] = Vertex(x)
    def vertices[A](xs: List[A]): Graph[A] = overlays(xs.map(vertex))

    def edge[A](x: A, y: A): Graph[A] = Connect(Vertex(x), Vertex(y))
    def edges[A](xs: List[(A, A)]): Graph[A] =
        overlays(xs.map { case (x, y) => edge(x, y) })

    def overlay[A](x: Graph[A], y: Graph[A]): Graph[A] = x + y
    def overlays[A](xs: List[Graph[A]]): Graph[A] =
        xs.foldRight(empty:Graph[A])(_+_)

    def connect[A](x: Graph[A], y: Graph[A]): Graph[A] = x * y
    def connects[A](xs: List[Graph[A]]): Graph[A] =
        xs.foldRight(empty:Graph[A])(_*_)
    def clique[A](xs: List[A]): Graph[A] = connects(xs.map(vertex))
    def biclique[A](xs: List[A], ys: List[A]): Graph[A] =
        connect(vertices(xs), vertices(ys))
    def star[A](x: A, ys: List[A]): Graph[A] =
        connect(vertex(x), vertices(ys))

    def path[A](xs: List[A]): Graph[A] = xs match {
        case Nil      => empty
        case x :: Nil => vertex(x)
        case xs       => edges(xs.zip(xs.tail))
    }
    def circuit[A](xs: List[A]): Graph[A] = xs match {
        case Nil     => empty
        case x :: xs => path(List(x) ++ xs ++ List(x))
    }

    def box[A,B](x: Graph[A], y: Graph[B]): Graph[(A, B)] = {
        val xs = y.toList.map { b => x.map { a => (a, b) } }
        val ys = x.toList.map { a => y.map { b => (a, b) } }
        overlays(xs ++ ys)
    }
    def mesh[A,B](xs: List[A], ys: List[B]): Graph[(A, B)] =
        box(path(xs), path(ys))
    def torus[A,B](xs: List[A], ys: List[B]): Graph[(A, B)] =
        box(circuit(xs), circuit(ys))
}

object App {
    def main(args : Array[String]) {
        val e = Graph.empty
        val x = Graph.vertex("x")
        val y = Graph.vertex("y")
        val xy = x * y
        val ee = e + e * e;
        val vs = Graph.vertices(List(1,2,3,4,5))
        println("e .isEmpty: " + e.isEmpty)
        println("x .isEmpty: " + x.isEmpty)
        println("xy.isEmpty: " + xy.isEmpty)
        println("ee.isEmpty: " + ee.isEmpty)
        println("vs              : " + vs)
        println("vs.map(_ + 1)   : " + vs.map(_ + 1))
        println("vs.induce(_ > 3): " + vs.induce(_ > 3))
        println("path   (List(2,1,3)): " + Graph.path(List(2,1,3)))
        println("circuit(List(2,1,3)): " + Graph.circuit(List(2,1,3)))
        println("isSubgraphOf(x, xy): " + x.isSubgraphOf(xy))
        println("isSubgraphOf(xy, x): " + xy.isSubgraphOf(x))
    }
}
