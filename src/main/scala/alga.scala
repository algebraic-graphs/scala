package alga

// TODO:
// trait GraphLike[A, +This] {
// }

sealed trait Graph[+A] {

    def +[B >: A](that: Graph[B]): Graph[B] = Overlay(this, that)
    def *[B >: A](that: Graph[B]): Graph[B] = Connect(this, that)

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
        flatMap({x => if (p(x)) Vertex(x) else Empty})

    def toList: List[A] = this match {
        case Empty         => Nil
        case Vertex(x)     => List(x)
        case Overlay(x, y) => x.toList ++ y.toList
        case Connect(x, y) => x.toList ++ y.toList
    }

    // TODO: Requires the right graph equality
    // def isSubgraphOf[A](x: Graph[A]): Boolean =
    //     Overlay(this, x) == x
}

case object Empty                                extends Graph[Nothing]
case class  Vertex [A](a: A                    ) extends Graph[A]
case class  Overlay[A](x: Graph[A], y: Graph[A]) extends Graph[A]
case class  Connect[A](x: Graph[A], y: Graph[A]) extends Graph[A]

// This can be turned into a generic interface
object Graph {
    def empty: Graph[Nothing] = Empty

    def vertex[A](x: A): Graph[A] = Vertex(x)
    def vertices[A](xs: List[A]): Graph[A] = overlays(xs.map(vertex))

    def edge[A](x: A, y: A): Graph[A] = Connect(Vertex(x), Vertex(y))
    def edges[A](xs: List[(A, A)]): Graph[A] =
        overlays(xs.map({case (x, y) => edge(x, y)}))

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
        val xs = y.toList.map({ b => x.map({a => (a, b)}) })
        val ys = x.toList.map({ a => y.map({b => (a, b)}) })
        overlays(xs ++ ys)
    }
    def mesh[A,B](xs: List[A], ys: List[B]): Graph[(A, B)] =
        box(path(xs), path(ys))
    def torus[A,B](xs: List[A], ys: List[B]): Graph[(A, B)] =
        box(circuit(xs), circuit(ys))
}

object App {
    def main(args : Array[String]) {
        val e = Empty
        val x = Vertex("x")
        val y = Vertex("y")
        val xy = Graph.edge(x, y)
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
        // println("isSubgrapOf(x, xy): " + x.isSubgraphOf(xy))
    }
}
