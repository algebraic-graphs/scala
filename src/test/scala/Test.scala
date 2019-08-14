import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import alga._

object DirectedGraphs extends Properties("Directed Graphs"){
  include(DirectedGraphAxioms)
  include(DirectedGraphTheorems)

  def genGraph: Gen[Graph[Int]] = {
    val genEmpty = Gen.const(Empty)

    val genVertex = for {
      v <- arbitrary[Int]
      other <- genGraph
    } yield Graph.connect(Vertex(v), other)

    Gen.oneOf(genEmpty, Gen.lzy(genVertex))
  }

  object DirectedGraphAxioms extends Properties("Axioms") {
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

  object DirectedGraphTheorems extends Properties("Theorems") {
    property("Overlay Identity") = forAll (genGraph) { (x: Graph[Int]) =>
      x + Empty == x
    }

    property("Overlay Idempotence") = forAll (genGraph) { (x: Graph[Int]) =>
      x + x == x
    }

    property("Absortion") = forAll (genGraph, genGraph, genGraph) { (x: Graph[Int], y: Graph[Int], z: Graph[Int]) =>
      x + y + x * y == x * y
    }

    property("Full Decomposition") = forAll (genGraph, genGraph, genGraph) { (x: Graph[Int], y: Graph[Int], z: Graph[Int]) =>
      x * y * z == x * y + x * z + y * z + x + y + z + Empty
    }

    property("Connect Saturation") = forAll (genGraph) { (x: Graph[Int]) =>
      x * x == x * x * x
    }

    property("Lower bound") = forAll (genGraph) { (x: Graph[Int]) =>
      Empty <= x
    }

    property("Overlay Order") = forAll (genGraph, genGraph) { (x: Graph[Int], y: Graph[Int]) =>
      x <= x + y
    }

    property("Overlay Connect Order") = forAll (genGraph, genGraph) { (x: Graph[Int], y: Graph[Int]) =>
      x + y <= x * y
    }
  }
}
