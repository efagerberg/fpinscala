package fpinscala.datastructures

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

class ListSpec extends AnyFunSpec {
  describe("x") {
    it("should equal 3") {
      assert(List.x == 3)
    }
  }

  describe("tail") {
    val scenarios = Table(
      ("x", "expected"),
      (List(1, 2, 3), List(2, 3)),
      (List(1), Nil),
      (List(3, 2, 1), List(2, 1))
    )

    forAll(scenarios) { (x: List[Int], expected: List[Int]) =>
      describe(s"when given ${x}") {
        it(s"returns ${expected}") {
          assert(List.tail(x) == expected)
        }
      }
    }

    describe("Given empty list") {
      val x = List()
      it("raises an error") {
        assertThrows[RuntimeException] { List.tail(x) }
      }
    }
  }

  describe("setHead") {
    val scenarios = Table(
      ("l", "n", "expected"),
      (List(), 1, List(1)),
      (List(2), 1, List(1, 2)),
      (List("B", "C"), "A", List("A", "B", "C"))
    )

    forAll(scenarios) { (l, n, expected) =>
      describe(s"Given ${l} and ${n}") {
        it(s"returns ${expected}") {
          assert(List.setHead(l, n) == expected)
        }
      }
    }
  }

  describe("drop") {
    val scenarios = Table(
      ("l", "n", "expected"),
      (List(1), 0, List(1)),
      (List(1, 2), 1, List(2)),
      (List(1, 2, 3), 3, Nil),
      (List(1, 2, 3, 4), 3, List(4))
    )

    forAll(scenarios) { (l, n, expected) =>
      describe(s"Given ${l} and ${n}") {
        it(s"returns ${expected}") {
          assert(List.drop(l, n) == expected)
        }
      }
    }
  }

  describe("dropWhile") {
    val scenarios = Table(
      ("l", "f", "expected"),
      (List(1, 2), (a: Int) => a % 2 == 0, List(1, 2)),
      (List(1, 2), (a: Int) => a % 2 != 0, List(2)),
      (List(3, 6, 9), (a: Int) => true, Nil),
      (List(1, 2, 3), (a: Int) => false, List(1, 2, 3))
    )

    forAll(scenarios) { (l, f, expected) =>
      describe(s"Given ${l} and ${f.hashCode()}") {
        it(s"returns ${expected}") {
          assert(List.dropWhile(l)(f) == expected)
        }
      }
    }
  }

  describe("init") {
    val scenarios = Table(
      ("l", "expected"),
      (List(1), Nil),
      (List(1, 2), List(1)),
      (List(1, 2, 3), List(1, 2))
    )

    forAll(scenarios) { (l, expected) =>
      describe(s"Given ${l}") {
        it(s"returns ${expected}") {
          assert(List.init(l) == expected)
        }
      }
    }

    describe(s"Given ${Nil}") {
      it("raises an error") {
        assertThrows[RuntimeException] { List.init(Nil) }
      }
    }
  }

  describe("length") {
    val scenarios = Table(
      ("l", "expected"),
      (Nil, 0),
      (List(1), 1),
      (List(2, 3), 2),
      (List(3, 4, 5), 3),
      (List(1, 2, 3, 4, 5, 6, 7, 8, 9), 9)
    )

    forAll(scenarios) { (l, expected) =>
      describe(s"GIven ${l}") {
        it(s"returns ${expected}") {
          assert(List.length(l) == expected)
        }
      }
    }
  }

  describe("foldLeft") {
    val scenarios = Table(
      ("l"),
      (List(1, 2, 3)),
      (List(3, -2, 1)),
      (List(2, 100, 4, 22))
    )

    forAll(scenarios) { (l) =>
      describe(s"Given a communative function and ${l}") {
        def sum = (a: Int, b: Int) => a + b

        it("Returns the same result as foldRight") {
          assert(List.foldLeft(l, 0)((x, y) => x + y) == List.sum2((l)))
        }
      }
    }
  }

  describe("sum3") {
    val scenarios = Table(
      ("l", "expected"),
      (List(1), 1),
      (List(1, 2), 3),
      (List(1, 2, 3), 6),
      (List(-100, 120, 10000), 10020)
    )

    forAll(scenarios) { (l, expected) =>
      describe(s"given ${l}") {
        it(s"returns ${expected}") {
          assert(List.sum3(l) == expected)
        }
      }
    }
  }

  describe("product3") {
    val scenarios = Table(
      ("l", "expected"),
      (List(1), 1),
      (List(1, 2), 2),
      (List(1, 2, 3), 6),
      (List(5, 10, 100), 5000)
    )

    forAll(scenarios) { (l, expected) =>
      describe(s"given ${l}") {
        it(s"returns ${expected}") {
          assert(List.product3(l) == expected)
        }
      }
    }
  }

  describe("length2") {
    val scenarios = Table(
      ("l", "expected"),
      (List(1), 1),
      (List(1, 2), 2),
      (List(1, 2, 3), 3),
      (List(5, 10, 100, 1000, 100000), 5)
    )

    forAll(scenarios) { (l, expected) =>
      describe(s"given ${l}") {
        it(s"returns ${expected}") {
          assert(List.length2(l) == expected)
        }
      }
    }
  }

  describe("reverse") {
    val scenarios = Table(
      ("l", "expected"),
      (List(1), List(1)),
      (List(1, 2), List(2, 1)),
      (List(1, 2, 3), List(3, 2, 1)),
      (List(1, 2, 3, 4), List(4, 3, 2, 1))
    )

    forAll(scenarios) { (l, expected) =>
      describe(s"given ${l}") {
        it(s"returns ${expected}") {
          assert(List.reverse(l) == expected)
        }
      }
    }
  }

  describe("foldRightViaFoldLeft") {
    val scenarios = Table(
      ("l"),
      (List(1, 2, 3)),
      (List(3, -2, 1)),
      (List(2, 100, 4, 22))
    )

    forAll(scenarios) { (l) =>
      describe(s"Given a sum function and ${l}") {
        def sum = (a: Int, b: Int) => a + b

        it("Returns the same result as foldRight") {
          assert(List.foldRightViaFoldLeft(l, 0)(_ + _) == List.sum2((l)))
        }
      }
    }
  }

  describe("append2") {
    val scenarios = Table(
      ("a1", "a2", "expected"),
      (List(1, 2, 3), List(5), List(1, 2, 3, 5)),
      (List(3, -2, 1), List(1, 1000), List(3, -2, 1, 1, 1000)),
      (
        List(2, 100, 4, 22),
        List(-92, -91, -90),
        List(2, 100, 4, 22, -92, -91, -90)
      )
    )

    forAll(scenarios) { (a1, a2, expected) =>
      describe(s"Given a ${a1} and ${a2}") {
        it(s"returns ${expected}") {
          assert(List.append2(a1, a2) == expected)
        }
      }
    }
  }

  describe("concat") {
    val scenarios = Table(
      ("l", "expected"),
      (List(Nil: List[Int]), Nil: List[Int]),
      (List(List(2, 3), List(4, 5)), List(2, 3, 4, 5)),
      (List(List(2, 3), List(1, 4, 5)), List(2, 3, 1, 4, 5))
    )

    forAll(scenarios) { (l, expected) =>
      describe(s"Given a ${l}") {
        it(s"returns ${expected}") {
          assert(List.concat(l) == expected)
        }
      }
    }
  }

  describe("add1") {
    val scenarios = Table(
      ("l", "expected"),
      (List(1, 2, 3, 4), List(2, 3, 4, 5)),
      (List(-1, 0, -10, -10), List(0, 1, -9, -9)),
      (List(999, 1, -999, 5), List(1000, 2, -998, 6))
    )

    forAll(scenarios) { (l, expected) =>
      describe(s"Given ${l}") {
        it(s"returns ${expected}") {
          assert(List.add1(l) == expected)
        }
      }
    }
  }

  describe("doublesToStrings") {
    val scenarios = Table(
      ("l", "expected"),
      (List(1.1, 2.10, -12), List("1.1", "2.1", "-12.0")),
      (List(1.0, 2.0), List("1.0", "2.0"))
    )

    forAll(scenarios) { (l, expected) =>
      describe(s"given ${l}") {
        it(s"returns ${expected}") {
          assert(List.doublesToStrings(l) == expected)
        }
      }
    }
  }

  describe("map") {
    val scenarios = Table(
      ("l", "expected"),
      (List(0), List(0)),
      (List(1, 2, 3), List(1, 4, 9)),
      (List(-2, -4, 10), List(4, 16, 100))
    )

    forAll(scenarios) { (l, expected) =>
      describe(s"when given ${l}") {
        describe(s"and given a square function") {
          def square = (a: Int) => (a * a)

          it(s"returns ${expected}") {
            assert(List.map(l)(square) == expected)
          }
        }
      }
    }
  }

  describe("filter") {
    val scenarios = Table(
      ("l", "f", "expected"),
      (List(0), (a: Int) => a > 0, Nil: List[Int]),
      (List(1, 2, 3), (a: Int) => a % 2 == 0, List(2)),
      (List(-2, -4, 10), (a: Int) => a < 0, List(-2, -4))
    )

    forAll(scenarios) { (l, f, expected) =>
      describe(s"given ${l} and ${f.hashCode}") {
        it(s"returns ${expected}") {
          assert(List.filter(l)(f) == expected)
        }
      }
    }
  }

  describe("flatMap") {
    val scenarios = Table(
      ("l", "f", "expected"),
      (List(1, 2, 3), ((i: Int) => List(i, i)), List(1, 1, 2, 2, 3, 3)),
      (List(-1, 2, -3), ((i: Int) => List(i, i + 1)), List(-1, 0, 2, 3, -3, -2))
    )

    forAll(scenarios) { (l, f, expected) =>
      describe(s"given ${l} and ${f.hashCode}") {
        it(s"returns ${expected}") {
          assert(List.flatMap(l)(f) == expected)
        }
      }
    }
  }

  describe("filter2") {
    val scenarios = Table(
      ("l", "f", "expected"),
      (List(0), (a: Int) => a > 0, Nil: List[Int]),
      (List(1, 2, 3), (a: Int) => a % 2 == 0, List(2)),
      (List(-2, -4, 10), (a: Int) => a < 0, List(-2, -4))
    )

    forAll(scenarios) { (l, f, expected) =>
      describe(s"given ${l} and ${f.hashCode}") {
        it(s"returns ${expected}") {
          assert(List.filter2(l)(f) == expected)
        }
      }
    }
  }

  describe("add") {
    val scenarios = Table(
      ("a1", "a2", "expected"),
      (List(1, 2), List(1, 2), List(2, 4)),
      (List(-10, 10), List(100, 201), List(90, 211)),
      (List(-100, -200), List(-100, -300), List(-200, -500))
    )

    forAll(scenarios) { (a1, a2, expected) =>
      describe(s"given ${a1} and ${a2}") {
        it(s"given ${expected}") {
          assert(List.add(a1, a2) == expected)
        }
      }
    }
  }

  describe("zipWith") {
    val scenarios = Table(
      ("l1", "l2", "f", "expected"),
      (List(1, 2), List(3, 4), (x: Int, y: Int) => x * y, List(3, 8)),
      (List(3, 4), List(1, 2), (x: Int, y: Int) => x - y, List(2, 2))
    )

    forAll(scenarios) { (l1, l2, f, expected) =>
      describe(s"given ${l1}, ${l2}, and ${f.hashCode}") {
        it(s"returns ${expected}") {
          assert(List.zipWith(l1)(l2)(f) == expected)
        }
      }
    }
  }

  describe("hasSubsequence") {
    val scenarios = Table(
      ("l1", "l2", "expected"),
      (List(1, 2, 3), List(4, 5, 6), false),
      (List(1, 2), List(1, 2), true),
      (List(1, 2), List(1), true),
      (List(1, 2, 3), Nil, true),
      (List(1, 2), List(3), false)
    )

    forAll(scenarios) { (l1, l2, expected) =>
      describe(s"given ${l1}, ${l2}") {
        it(s"returns ${expected}") {
          assert(List.hasSubsequence(l1, l2) == expected)
        }
      }
    }
  }
}
