import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import alga._

object DirectedGraphAxioms extends Properties("Axioms") {
  val genEmpty = Gen.const(Empty)

  val genVertex = for {
    v <- arbitrary[Int]
    other <- genGraph
  } yield Graph.connect(Vertex(v), other)

  val genGraph: Gen[Graph[Int]] = Gen.oneOf(genEmpty, Gen.lzy(genVertex))

  property("Overlay Commutativity") = forAll (genGraph, genGraph) { (x: Graph[Int], y: Graph[Int]) =>
    x + y == y + x
  }

  property("Overlay Associativity") = forAll (genGraph, genGraph, genGraph) { (x: Graph[Int], y: Graph[Int], z: Graph[Int]) =>
    x + (y + z) == (x + y) + z
  }

  property("Left Connect Identity") = forAll (genGraph) { (x: Graph[Int]) =>
    Empty * x == x
  }

  property("Right Connect Identity") = forAll (genGraph) { (x: Graph[Int]) =>
    x * Empty == x
  }

  property("Connect Associativity") = forAll (genGraph, genGraph, genGraph) { (x: Graph[Int], y: Graph[Int], z: Graph[Int]) =>
    x * (y * z) == (x * y) * z
  }

  property("Left Distributivity") = forAll (genGraph, genGraph, genGraph) { (x: Graph[Int], y: Graph[Int], z: Graph[Int]) =>
    x * (y + z) == x * y + x * z
  }

  property("Right Distributivity") = forAll (genGraph, genGraph, genGraph) { (x: Graph[Int], y: Graph[Int], z: Graph[Int]) =>
    (x + y) * z == x * z + y * z
  }

  property("Decomposition") = forAll (genGraph, genGraph, genGraph) { (x: Graph[Int], y: Graph[Int], z: Graph[Int]) =>
    x * y * z == x * y + x * z + y * z
  }
}
