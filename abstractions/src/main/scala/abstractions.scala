package abstractions

// - Monad & stuff -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Standard categorical abstractions. Nothing special to see there.
trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]

object Functor:
  given [Left]: Functor[Tuple2[Left, _]] with
    extension [A](fa: (Left, A)) def map[B](f: A => B) = (fa._1, f(fa._2))

trait Apply[F[_]] extends Functor[F]:
  extension [A, B](ff: F[A => B]) def ap: F[A] => F[B]

trait Applicative[F[_]] extends Apply[F]:
  extension [A](a: A) def pure: F[A]

trait FlatMap[F[_]] extends Functor[F]:
  extension [A](fa: F[A]) def flatMap[B](f: A => F[B]): F[B]

trait Monad[F[_]] extends Applicative[F] with FlatMap[F]:
  extension [A, B](ff: F[A => B]) def ap = fa => fa.flatMap(a => ff.map(_.apply(a)))

// Natural transformations (functions from F[A] to G[A])
trait ~>[F[_], G[_]]:
  def apply[A](fa: F[A]): G[A]

// - Id ----------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
opaque type Id[A] >: A = A

extension [A](id: Id[A]) def unwrap: A = id

object Id:
  given Monad[Id] with
    extension [A](a: A) def pure = a

    extension [A](fa: Id[A])
      def map[B](f: A => B)         = f(fa)
      def flatMap[B](f: A => Id[B]) = f(fa)

// - Instances ---------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object Monad:
  given Monad[Option] with
    extension [A](a: A) def pure = Some(a)

    extension [A](fa: Option[A])
      def map[B](f: A => B)             = fa.map(f)
      def flatMap[B](f: A => Option[B]) = fa.flatMap(f)

  given Monad[List] with
    extension [A](a: A) def pure = List(a)

    extension [A](fa: List[A])
      def map[B](f: A => B)           = fa.map(f)
      def flatMap[B](f: A => List[B]) = fa.flatMap(f)
