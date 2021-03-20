package fpinscala.datastructures

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

class TreeSpec extends AnyFunSpec {
  describe("size") {
    val scenarios = Table(
      ("t", "expected"),
      (Branch(Leaf(1), Leaf(0)), 3),
      (
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
        5
      )
    )

    forAll(scenarios) { (t, expected) =>
      describe(s"given ${t}") {
        it(s"returns ${expected}") {
          assert(Tree.size(t) == expected)
        }
      }
    }
  }

  describe("max") {
    val scenarios = Table(
      ("t", "expected"),
      (Branch(Leaf(1), Leaf(0)), 1),
      (
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
        3
      )
    )

    forAll(scenarios) { (t, expected) =>
      describe(s"given ${t}") {
        it(s"returns ${expected}") {
          assert(Tree.max(t) == expected)
        }
      }
    }
  }

  describe("depth") {
    val scenarios = Table(
      ("t", "expected"),
      (Branch(Leaf(1), Leaf(0)), 2),
      (
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
        2
      )
    )

    forAll(scenarios) { (t, expected) =>
      describe(s"Given ${t}") {
        it(s"returns ${expected}") {
          assert(Tree.depth(t) == expected)
        }
      }
    }
  }

  describe("map") {
    val scenarios = Table(
      ("t", "f", "expected"),
      (
        Branch(Leaf(1), Leaf(0)),
        (a: Int) => a + 1,
        Branch(Leaf(2), Leaf(1))
      ),
      (
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
        (a: Int) => a * 3,
        Branch(Leaf(3), Branch(Leaf(6), Leaf(9)))
      )
    )

    forAll(scenarios) { (t, f, expected) =>
      describe(s"given ${t} and ${f.hashCode}") {
        it(s"returns ${expected}") {
          assert(Tree.map(t)(f) == expected)
        }
      }
    }
  }

  describe("fold") {
    val scenarios = Table(
      ("t", "f", "g", "expected"),
      (
        Branch(Leaf(1), Leaf(0)),
        (a: Int)  => 1,
        (a: Int, b: Int) => (1 + a + b),
        3
      ),
      (
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
        (a: Int) => a,
        (a: Int, b: Int) => a max b,
        3
      ),
      (
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
        (a: Int) => 1,
        (a: Int, b: Int) => 1 + a max b,
        2
      )
    )

    forAll(scenarios) { (t, f, g, expected) =>
      describe(s"given ${t}, ${f.hashCode} and ${g.hashCode}") {
        it(s"returns ${expected}") {
          assert(Tree.fold(t)(f)(g) == expected)
        }
      }
    }
  }
}
