/** Scala implementation of the concepts described in Datatypes à la carte:
  * https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409
  */

// - Generic type classes ----------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]

// - Coproduct ---------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Very much like `Either`, but for higher kinded types.
  *
  * `Either[A, B]` only works for concrete `A` and `B` types. We want something that allows us to express
  * `Either[Option, List]`, for example.
  *
  * `Coproduct` does this, but we also introduce the `:+:` alias for convenience: `(Option :+: List)[A]` is equivalent
  * to `Coproduct[Option, List, A]`. You can of course pass `Option :+: List` wherever a type constructor is expected.
  */
enum Coproduct[F[_], G[_], E]:
  case Inl(value: F[E])
  case Inr(value: G[E])
import Coproduct.*

object Coproduct:
  /** The coproduct of `F` and `G` is a functor provided by `F` and `G` are. */
  given [F[_]: Functor, G[_]: Functor]: Functor[F :+: G] with
    extension [A](fga: (F :+: G)[A])
      def map[B](f: A => B) = fga match
        case Inl(fa) => Inl(fa.map(f))
        case Inr(ga) => Inr(ga.map(f))

type :+:[F[_], G[_]] = [A] =>> Coproduct[F, G, A]

// - Injection ---------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Describes the ability to treat a type constructor `Sub` as if it were another type constructor `Sup`.
  *
  * This feels a lot like subtyping for type constructors: Given `Sub :<: Sup`, we can:
  *   - always use a `Sub[A]` where a `Sup[A]` is expected.
  *   - sometimes us a `Sup[A]` where a `Sub[A]` is expected, if we have the "right" `Sup`.
  *
  * It's also extremely similar to a prism, which allows us to reach inside a sum type and get / set values if we're in
  * the right variant.
  */
trait :<:[Sub[_], Sup[_]]:
  self =>
  extension [A](sub: Sub[A]) def inject: Sup[A]
  extension [A](sup: Sup[A]) def project: Option[Sub[A]]

  def andThen[SSup[_]](using s2ss: Sup :<: SSup): Sub :<: SSup = new (Sub :<: SSup):
    extension [A](sub: Sub[A]) def inject = s2ss.inject(self.inject(sub))
    extension [A](sup: SSup[A])
      def project = for
        g <- s2ss.project(sup)
        f <- self.project(g)
      yield f

object :<: :
  /** You can always inject a type constructor in itself. */
  given refl[F[_]]: (F :<: F) with
    extension [A](sub: F[A]) def inject  = sub
    extension [A](sup: F[A]) def project = Some(sup)

  /** You can always inject `F` in `F :+: G`, by simply mapping it to the left branch. */
  given coprodLeft[F[_], G[_]]: (F :<: (F :+: G)) with
    extension [A](sub: F[A]) def inject = Inl(sub)
    extension [A](sup: (F :+: G)[A])
      def project = sup match
        case Inl(fa) => Some(fa)
        case Inr(_)  => None

  /** You can always inject `F` in `G :+: F`, by simply mapping it to the right branch. */
  given coprodRight[F[_], G[_]]: (F :<: (G :+: F)) with
    extension [A](sub: F[A]) def inject = Inr(sub)
    extension [A](sup: (G :+: F)[A])
      def project = sup match
        case Inr(fa) => Some(fa)
        case Inl(_)  => None

  given lolwat[F[_], G[_], H[_]](using fg: F :<: G): (F :<: (H :+: G)) = fg.andThen[H :+: G]

// - Eval --------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
trait Eval[F[_]]:
  extension (fi: F[Int]) def eval: Int

object Eval:
  given [F[_]: Eval, G[_]: Eval]: Eval[F :+: G] with
    extension (value: (F :+: G)[Int])
      def eval = value match
        case Inl(fi) => fi.eval
        case Inr(gi) => gi.eval

// - Render ------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
trait Render[F[_]]:
  extension [G[_]: Render](fs: F[Expr[G]]) def render: String

object Render:
  given coproduct[F[_]: Render, G[_]: Render]: Render[F :+: G] with
    extension [H[_]: Render](value: (F :+: G)[Expr[H]])
      def render = value match
        case Inl(fi) => fi.render
        case Inr(gi) => gi.render

// - Expr --------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** `Expr` is really just `Fix` with a different name.
  *
  * We get there by first implementing i
  */
case class Expr[F[_]](in: F[Expr[F]]):
  def fold[A](f: F[A] => A)(using Functor[F]): A =
    f(in.map(_.fold(f)))

  def eval(using Functor[F], Eval[F]): Int        = fold(fa => fa.eval)
  def pretty(using Functor[F], Render[F]): String = in.render

  // This is not in the original paper. Being able to turn an Expr[F] into an Expr[F :+: G] is useful to compose
  // expressions of different (but compatible) dialects.
  // Imagine that you have an Expr[Value] and an Expr[Value :+: Add] that you need to add together.
  // You can use leftIn[Add] to turn the first into an Expr[Value :+: Add] and get compatible types.
  def leftIn[G[_]](using Functor[F]): Expr[F :+: G] = Expr(Inl(in.map(_.leftIn)))

object Expr:
  def inject[F[_], G[_]](expr: G[Expr[F]])(using G :<: F): Expr[F] =
    Expr(expr.inject)

// - Val ---------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
case class Val[A](value: Int)
object Val:
  given Functor[Val] with
    extension [A](fa: Val[A]) def map[B](f: A => B) = Val(fa.value)

  given Eval[Val] with
    extension (value: Val[Int]) def eval = value.value

  given Render[Val] with
    extension [G[_]: Render](value: Val[Expr[G]]) def render = value.value.toString

def value[F[_]](i: Int)(using Val :<: F): Expr[F] =
  Expr.inject(Val(i))

// - Add ---------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
case class Add[A](lhs: A, rhs: A)
object Add:
  given Functor[Add] with
    extension [A](fa: Add[A]) def map[B](f: A => B) = Add(f(fa.lhs), f(fa.rhs))

  given Eval[Add] with
    extension (add: Add[Int]) def eval = add.lhs + add.rhs

  given Render[Add] with
    extension [G[_]: Render](add: Add[Expr[G]])
      def render =
        s"${add.lhs.in.render} + ${add.rhs.in.render}"

def add[F[_]](lhs: Expr[F], rhs: Expr[F])(using Add :<: F): Expr[F] =
  Expr.inject(Add(lhs, rhs))

// - Mult --------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
case class Mult[A](lhs: A, rhs: A)
object Mult:
  given Functor[Mult] with
    extension [A](fa: Mult[A]) def map[B](f: A => B) = Mult(f(fa.lhs), f(fa.rhs))

  given Eval[Mult] with
    extension (mult: Mult[Int]) def eval = mult.lhs * mult.rhs

  given Render[Mult] with
    extension [G[_]: Render](mult: Mult[Expr[G]])
      def render =
        s"${mult.lhs.in.render} * ${mult.rhs.in.render}"

def mult[F[_]](lhs: Expr[F], rhs: Expr[F])(using Mult :<: F): Expr[F] =
  Expr.inject(Mult(lhs, rhs))

// - Tests -------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

val addExample: Expr[Val :+: Add]           = Expr(Inr(Add(Expr(Inl(Val(118))), Expr(Inl(Val(1219))))))
val addExample2: Expr[Val :+: Add]          = add(value(3000), add(value(1330), value(7)))
val addExample3: Expr[Val :+: Add :+: Mult] = mult(value(2), add(value(3), value(4)))

@main def run =
  println(addExample.eval)
  println(addExample2.eval)
  println(addExample3.eval)
  println(addExample3.pretty)
