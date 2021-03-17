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
}
