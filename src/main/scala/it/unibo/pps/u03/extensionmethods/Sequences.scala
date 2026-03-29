package it.unibo.pps.u03.extensionmethods

import scala.annotation.tailrec

object Sequences:
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension (l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), t.map(mapper))
        case Nil()      => Nil()

      def filter(pred: A => Boolean): Sequence[A] = l match
        case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
        case Cons(_, t)            => t.filter(pred)
        case Nil()                 => Nil()

      def concat(s2: Sequence[A]): Sequence[A] = 
        def _concat(s1: Sequence[A])(sequence: Sequence[A]): Sequence[A] = (s1, s2) match
          case (s1, Nil()) => s1
          case (Nil(), s2) => s2
          case (Cons(h1, t1), s2) => Cons(h1, _concat(t1)(s2))
        _concat(l)(s2)

      def reverse(s: Sequence[A]): Sequence[A] =
        @tailrec
        def _reverse[B](s: Sequence[B])(acc: Sequence[B]): Sequence[B] = s match
          case Nil() => Nil()
          case Cons(h, Nil()) => Cons(h, acc)
          case Cons(h, t) => _reverse(t)(Cons(h, acc))
        _reverse(s)(Nil())

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = 
        def _flatMap(s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = (s, mapper) match
          case (Nil(), m) => Nil()
          case (Cons(h, t), m) => m(h).concat(_flatMap(t)(m))
        _flatMap(l)(mapper)

      @tailrec
      def contains[B](s: Sequence[B])(elem: B): Boolean = s match
        case Nil() => false
        case Cons(h, _) if h.equals(elem) => true
        case Cons(_, t) => contains(t)(elem)

      def distinct(): Sequence[A] =
        @tailrec
        def _distinct[B](s: Sequence[B])(acc: Sequence[B]): Sequence[B] = s match
          case Nil() => acc
          case Cons(h, t) if contains(acc)(h) => _distinct(t)(acc)
          case Cons(h, t) => _distinct(t)(Cons(h, acc))
        reverse(_distinct(l)(Nil()))

    def of[A](n: Int, a: A): Sequence[A] =
      if (n == 0) then Nil[A]() else Cons(a, of(n - 1, a))

@main def trySequences() =
  import Sequences.*
  import Sequence.*
  
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  println(seq.filter(_ >= 20).map(_ + 1).sum) // 21+31 = 52
  println(sum(map(filter(seq)(_ >= 20))(_ + 1))) // equally possible
  val seq2 = of(10, -1) // Cons(-1, Cons(-1, Cons(-1, ...)))
  println(seq2.sum) // -10
  
