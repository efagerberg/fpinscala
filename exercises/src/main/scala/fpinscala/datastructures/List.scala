package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil
    extends List[
      Nothing
    ] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](
      as: List[A],
      z: B
  )(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil     => sys.error("tail of empty list")
    case default => drop(l, 1)
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => List(h)
    case Cons(x, xs) => Cons(h, Cons(x, xs))
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil        => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _                   => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b, a) => f(a, b))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRightViaFoldLeft(a1, a2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRightViaFoldLeft(l, Nil: List[A])(append)

  def add1(l: List[Int]): List[Int] =
    foldRightViaFoldLeft(l, Nil: List[Int])((h, acc) => Cons(h + 1, acc))

  def doublesToStrings(l: List[Double]): List[String] =
    foldRightViaFoldLeft(l, Nil: List[String])((h, acc) =>
      Cons(h.toString(), acc)
    )

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((h, acc) => Cons(f(h), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil: List[A])((h, t) =>
      if (f(h)) Cons(h, t) else t
    )

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def add(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, add(t1, t2))
  }

  def zipWith[A, B, C](l1: List[A])(l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1)(t2)(f))
    }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil)                              => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _                                     => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                       => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t)                => hasSubsequence(t, sub)
  }
}
