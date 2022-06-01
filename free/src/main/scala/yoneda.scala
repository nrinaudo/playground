package yoneda

import abstractions.*
import abstractions.Monad.given

trait CoYoneda[F[_], A]:
  self =>

  type Source
  val source: F[Source]
  val transformation: Source => A

  def map[B](f: A => B): CoYoneda[F, B] = new CoYoneda[F, B]:
    type Source = self.Source
    val transformation = self.transformation andThen f
    val source         = self.source
  def run(using Functor[F[_]]): F[A] = source.map(transformation)

object CoYoneda:
  def lift[F[_], A](fa: F[A]): CoYoneda[F, A] = new CoYoneda[F, A]:
    type Source = A
    val source         = fa
    val transformation = identity

  given [F[_]]: Functor[CoYoneda[F, _]] with
    extension [A](fa: CoYoneda[F, A]) def map[B](f: A => B) = fa.map(f)

trait Yoneda[F[_], A]:
  self =>
  def transformation[B](f: A => B): F[B]

  def map[B](f: A => B): Yoneda[F, B] = new Yoneda[F, B]:
    override def transformation[C](g: B => C) = self.transformation(f andThen g)

  def run: F[A] = transformation(identity)

object Yoneda:
  def lift[F[_]: Functor, A](fa: F[A]): Yoneda[F, A] = new Yoneda[F, A]:
    override def transformation[B](f: A => B) = fa.map(f)

@main def testYoneda =
  val list   = (0 to 50000).toList
  val lifted = Yoneda.lift(list)
  val fooed  = CoYoneda.lift(list)

  println("Starting lifted...")
  var time = System.currentTimeMillis
  val r1   = lifted.map(_ + 1).map(_ - 1).map(_ + 1).run
  println(s"Done: ${System.currentTimeMillis - time}")

  println("Starting normal...")
  time = System.currentTimeMillis
  val r2 = list.map(_ + 1).map(_ - 1).map(_ + 1)
  println(s"Done: ${System.currentTimeMillis - time}")

  println("Starting CoYoneda...")
  time = System.currentTimeMillis
  val r3 = fooed.map(_ + 1).map(_ - 1).map(_ + 1).run
  println(s"Done: ${System.currentTimeMillis - time}")

  println(r1 == r2)
  println(r2 == r3)
