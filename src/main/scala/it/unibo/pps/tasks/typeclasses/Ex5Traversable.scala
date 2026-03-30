package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.Optionals.Optional
import Optional.*

/*  Exercise 5: 
 *  - Generalize by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a data structure T[A]
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalization of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def apply[A](t: T[A])(f: A => Unit)(orElse: () => Unit): Unit

  given Traversable[Sequence] with
    def apply[A](s: Sequence[A])(f: A => Unit)(orElse: () => Unit): Unit = s match
      case Nil() => orElse()
      case Cons(h, t) =>
        f(h)
        apply(t)(f)(orElse)

  given Traversable[Optional] with
    def apply[A](o: Optional[A])(f: A => Unit)(orElse: () => Unit): Unit = o match
      case Empty() => orElse()
      case Just(v) => f(v)

  trait Loggable[T[_]]:
    def log[A](a: A): Unit
    def orElseLog(): Unit

  given Loggable[Sequence] with
    def log[A](a: A): Unit = println(s"The current element of the sequence is: $a")
    def orElseLog(): Unit = println("Sequence end\n")

  given Loggable[Optional] with
    def log[A](a: A): Unit = println(s"Optional has a value and the value is: $a\n")
    def orElseLog(): Unit = println("Optional is empty\n")

  def logAll[T[_]: {Traversable, Loggable}, A](t: T[A]): Unit =
    summon[Traversable[T]].apply(t)(summon[Loggable[T]].log)(summon[Loggable[T]].orElseLog)

  @main def tryLogging(): Unit =
    val emptyOpt = Empty()
    logAll(emptyOpt)

    val filledOpt = Just(3)
    logAll(filledOpt)

    val emptySeq = Nil()
    logAll(emptySeq)

    val filledSeq = Cons(1, Cons(2, Cons(3, Nil())))
    logAll(filledSeq)
