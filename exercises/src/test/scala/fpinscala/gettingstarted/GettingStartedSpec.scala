package fpinscala.gettingstarted

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

class GettingStartedSpec extends AnyFunSpec {
  describe("fib") {
    val scenarios = Table(
      ("input", "expected"),
      (0, 0),
      (1, 1),
      (2, 1),
      (3, 2),
      (4, 3),
      (5, 5),
      (6, 8),
      (7, 13),
      (8, 21),
      (9, 34)
    )
    forAll(scenarios) { (input: Int, expected: Int) =>
      describe(s"Given: ${input}") {
        it(s"returns: ${expected}") {
          assert(MyModule.fib(input) == expected)
        }
      }
    }
  }

  describe("isSorted") {
    val scenarios = Table(
      ("as", "gt", "expected"),
      (Array(1, 2, 3), (a: Int, b: Int) => a < b, true),
      (Array(3, 2, 1), (a: Int, b: Int) => a > b, true),
      (Array(1, 2, 3), (a: Int, b: Int) => a > b, false),
      (Array(1, 2, 3), (a: Int, b: Int) => a != b, true),
      (Array(1, 1, 2), (a: Int, b: Int) => a == b, false),
    )

    forAll(scenarios) { (as, gt, expected) =>
      describe(s"Given: [${as.mkString(", ")}] and a lambda: ${gt.hashCode()}") {
        it(s"returns ${expected}") {
          assert(PolymorphicFunctions.isSorted(as, gt) == expected)
        }
      }
    }
  }
}
