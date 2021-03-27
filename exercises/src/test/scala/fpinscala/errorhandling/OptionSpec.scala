package fpinscala.errorhandling

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

class OptionSpec extends AnyFunSpec {
  describe("map") {
    val scenarios = Table(
      ("x", "f", "expected"),
      (None, (a: Int) => a + 1, None),
      (Some(1), (a: Int) => a + 1, Some(2)),
      (None, (a: Int) => a / 10, None),
      (Some(1), (a: Int) => a / 10, Some(1 / 10))
    )

    forAll(scenarios) { (x, f, expected) =>
      describe(s"given ${x} and ${f.hashCode}") {
        it(s"returns ${expected}") {
          assert(x.map(f) == expected)
        }
      }
    }
  }

  describe("getOrElse") {
    val scenarios = Table(
      ("x", "default", "expected"),
      (None, "foo", "foo"),
      (None, "bar", "bar"),
      (Some(1), 100, 1),
      (Some(100), 1, 100)
    )

    forAll(scenarios) { (x, default, expected) =>
      describe(s"given ${x} and a default ${default}") {
        it(s"returns ${expected}") {
          assert(x.getOrElse(default) == expected)
        }
      }
    }
  }

  describe("flatMap") {
    val scenarios = Table(
      ("x", "f", "expected"),
      (None, (a: Int) => Some(a + 1), None),
      (Some(120), (a: Int) => Some(a + 1), Some(121))
    )

    forAll(scenarios) { (x, f, expected) =>
      describe(s"given ${x} and a ${f.hashCode}") {
        it(s"returns ${expected}") {
          assert(x.flatMap(f) == expected)
        }
      }
    }
  }

  describe("orElse") {
    val scenarios = Table(
      ("x", "default", "expected"),
      (None, Some("foo"), Some("foo")),
      (None, Some("bar"), Some("bar")),
      (Some(1), Some(100), Some(1)),
      (Some(100), Some(1), Some(100))
    )

    forAll(scenarios) { (x, default, expected) =>
      describe(s"given ${x} and a default ${default}") {
        it(s"returns ${expected}") {
          assert(x.orElse(default) == expected)
        }
      }
    }
  }

  describe("filter") {
    val scenarios = Table(
      ("x", "f", "expected"),
      (Some(0), (a: Int) => a > 0, None),
      (Some(2), (a: Int) => a % 2 == 0, Some(2)),
      (Some(-2), (a: Int) => a < 0, Some(-2))
    )

    forAll(scenarios) { (x, f, expected) =>
      describe(s"given ${x} and a ${f.hashCode}") {
        it(s"returns ${expected}") {
          assert(x.filter(f) == expected)
        }
      }
    }
  }
}
