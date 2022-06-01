package free.applicative

import abstractions.*

enum FreeApplicative[F[_], A]:
  case Pure(a: A)
  case Ap[F[_], X, A](fx: F[X], fxa: FreeApplicative[F, X => A]) extends FreeApplicative[F, A]

object FreeApplicative:
  def lift[F[_], A](fa: F[A]): FreeApplicative[F, A] =
    FreeApplicative.Ap(fa, FreeApplicative.Pure(identity))

  given [F[_]]: Applicative[FreeApplicative[F, _]] with
    extension [A](a: A)
      def pure: FreeApplicative[F, A] =
        FreeApplicative.Pure(a)

    extension [A](fa: FreeApplicative[F, A])
      def map[B](f: A => B) = fa match
        case FreeApplicative.Pure(a) => FreeApplicative.Pure(f(a))
        case FreeApplicative.Ap(fx, fxa) =>
          FreeApplicative.Ap(
            fx,
            fxa.map(xa => xa andThen f)
          )

    extension [A, B](ff: FreeApplicative[F, A => B])
      def ap: FreeApplicative[F, A] => FreeApplicative[F, B] = ff match
        case FreeApplicative.Pure(f) => fa => fa.map(f)
        case FreeApplicative.Ap(fx, fxa2b): FreeApplicative.Ap[F, x, A => B] =>
          val flipped = fxa2b.map(xa2b => (a: A) => (x: x) => xa2b(x)(a))
          fa =>
            val foo: FreeApplicative[F, A => x => B] = fxa2b.map(xa2b => a => x => xa2b(x)(a))
            FreeApplicative.Ap(
              fx,
              flipped.ap(fa)
            )
