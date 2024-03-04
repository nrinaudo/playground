package introduction

import java.io.File
import java.nio.file.Files
import cats.effect.IO
import cats.effect.std.Console

// - Generic type classes ----------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]

trait Monad[F[_]] extends Functor[F]:
  extension [A](a: A) def pure: F[A]
  extension [A, B](fa: F[A]) def flatMap(f: A => F[B]): F[B]

// - Basic problem -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/* Basic expression, leading us immediately to the expression problem: we cannot add, say, `Mult`, without
 * changing `Expr` itself.
 */
object init:
  enum Expr:
    case Value(i: Int)
    case Add(lhs: Expr, rhs: Expr)

  def evaluate(expr: Expr): Int = expr match
    case Expr.Value(i)      => i
    case Expr.Add(lhs, rhs) => evaluate(lhs) + evaluate(rhs)

  def render(expr: Expr): String = expr match
    case Expr.Value(i)      => i.toString
    case Expr.Add(lhs, rhs) => s"${render(lhs)} + ${render(rhs)}"

// - Splitting Value & Add ---------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/* One solution is to decorelate `Expr`'s variants.
 * The intuition is that we can then write a new `Expr` implementation that somehow allows us to combine them back -
 * and potentially others with it.
 */
object splitting:
  // Value is trivial, it's a single value.
  case class Value(i: Int)

  // Add is more complicated: it needs operands, and what type should they be?
  // We want them to be at leats both `Value` and `Add` to be able to write `1 + (2 + 3)`. But we do not want to
  // correlate `Add` and `Value`. So we'll just make that a parameter and allow callers to add whatever makes sense
  // to them.
  case class Add[A](lhs: A, rhs: A)

  // We now want a type `Expr` that allows us to express something like `Expr[Add | Value]`: an expression composed of
  // either `Add` or `Value` statements. This should allow us to update that later to `Expr[Add | Value | Mult]`.
  // Clearly, we need `Expr` to be parametric.
  // We could, for example, write:
  case class ExprV1[A](in: A)

  // This makes it possible to write an expression composed strictly of values:
  type ExprV1Value = ExprV1[Value]

  // We cannot, however, do the same for `Add`: the kinds don't line up. `Add` is a type constructor, and `ExprV1`
  // takes a concrete type. So we'll want something like this instead:
  // case class Expr[F[_]](in: F[???])
  // The problem is the type of `in`: what to put in that `F[???]`?
  // Here's the intuition: an expression can be recursive (`Add` is, for example): it should be able to contain sub-
  // expressions of the same type as itself. For example, an `Expr[Value | Add]` should be able to contain sub-
  // expressions of type `Expr[Value | Add]`, exactly like our original `Expr` could contain sub-expressions of type
  // Expr.
  // So the type, weird though it is, is:
  case class Expr[F[_]](in: F[Expr[F]])

  // This reads: an expression whose list of operators is `F` is composed of an operator F whose operands are of type
  // Expr[F].
  // For `Add`:
  type ExprAdd = Expr[Add]

// We can no longer write `ExprValue` though - `Value` is a concrete type and `Expr` takes a type constructor...

// - Splitting Value & Add, step 2 -------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/* The trick here is to do the very human thing of saying _right, you want to give me a parameter I don't need? Fine,
 * I'll take it, but you can't make me use it!_.
 */
object splitting2:
  // Copy / pasting what we need from the previous step.
  case class Expr[F[_]](in: F[Expr[F]])
  case class Add[A](lhs: A, rhs: A)
  type ExprAdd = Expr[Add]

  // See? `Value` has a type parameter that it ignores. Which makes sense: we've said that this type parameter was
  // used to encode the operators that can be used by sub-expressions. `Value` cannot have sub-expressions, as it's
  // by definition a leaf of our syntax tree.
  case class Value[A](i: Int)
  type ExprValue = Expr[Value]

// - Grouping Value & Add ----------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/* We now have a working `Expr` type, as well as both `Add` and `Value`. We can express an expression composed strictly
 * of additions, or strictly of values, but we don't yet know how to combine them.
 *
 * The intuition is: we'd love to be able to do something like `Expr[Either[Value, Add]]`. But, again, the kinds don't
 * line up: `Either` takes concrete types and `Value` and `Add` are type constructors. We need to write some sort
 * of higher-kinded either, which we'll call `Coproduct`:
 */
object grouping:
  import splitting2.*

  enum Coproduct[Left[_], Right[_], A]:
    case Inl(value: Left[A])
    case Inr(value: Right[A])

  // This is very inconvenient to use, however. What goes in the ??? here?
  // type FullExpr = Expr[Coproduct[Add, Value, ???]]

  // We can do a little better with a type alias:
  type :+:[Left[_], Right[_]] = [A] =>> Coproduct[Left, Right, A]

  // We can now create values - here, for example 118 + 1219
  import Coproduct.*
  val lhs: Expr[Value :+: Add] =
    Expr(Inl(Value(118)))
  val rhs: Expr[Value :+: Add] =
    Expr(Inl(Value(1219)))
  val expr: Expr[Value :+: Add] = Expr(Inr(Add(lhs, rhs)))

// There's two things we need to do now:
// - gain back the ability to evaluate expr.
// - find a way to make this less of a nightmare to write, because `expr` is *not* friendly.

// - Evaluation --------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/* We can now represent expressions composed of additions and raw values. Which means we're almost back to where we
 * were, except we can't easily write evaluators for it.
 * How could we write an evaluator for `Expr[F[_]]`, for some `F`?
 * The easy answer is that `Expr` is basically `Fix` with a different name, so of course we'll be using a catamorphism.
 * This is boring though - and is sort of what the paper is doing. We should justify this choice.
 *
 * This would be easier with a diagram, but we're essentially trying to go from an `Expr[F]` to some `A`, where `A`
 * could be:
 *  - `Int` (for a regular evaluator).
 *  - `String` (for a pretty printer).
 *
 * Unwrapping our `Expr`, we get an `F[Expr[F]]`. The intuition here is that there's nothing we know about `F`, so
 * there's nothing we can do with that. We can't reach inside of it and get the `Expr[F]` that's inside. The most
 * common, simplest we know how working a value inside of an `F` is if that `F` is a functor, so let's pretend it is.
 * We're back to working with an `Expr[F]` - which we know how to work with! We're currently working on an
 * `Expr[F] => A`, so let's use that! which allows us to turn our `F[Expr[F]]` into an `F[A]`. We can't get from
 * an `F[A]` to the desired `A`, so we can cheat and say _give me a way to do that_. This is reasonable: there needs
 * to be some moving part, otherwise all the operators would be exactly the same. So we can ask callers to tell us,
 * in the context of that specific `F` and what they're trying to achieve.
 * This F-Algebra allows us, then, to go from an `Expr[F]` to an `A` - provided `F` is a functor.
 */
object evaluation:
  case class Expr[F[_]](in: F[Expr[F]]):
    def fold[A](algebra: F[A] => A)(using Functor[F]): A =
      algebra(in.map(_.fold(algebra)))

  // We can quite easily write an F-algebra for `Value` and `Add`:
  case class Add[A](lhs: A, rhs: A)
  case class Value[A](i: Int)

  def valueEval(value: Value[Int]): Int = value.i
  def addEval(add: Add[Int]): Int       = add.lhs + add.rhs

  // Calling them would be a `fold` on `Expr`, for which we need `Functor` instances:
  given Functor[Value] with
    extension [A](fa: Value[A]) def map[B](f: A => B) = Value(fa.i)
  given Functor[Add] with
    extension [A](fa: Add[A]) def map[B](f: A => B) = Add(f(fa.lhs), f(fa.rhs))

  // But what we really want is to write that for `Add :+: Value`. We can easily do that:
  enum Coproduct[Left[_], Right[_], A]:
    case Inl(value: Left[A])
    case Inr(value: Right[A])
  type :+:[Left[_], Right[_]] = [A] =>> Coproduct[Left, Right, A]

  def fullEval(expr: (Value :+: Add)[Int]): Int = expr match
    case Coproduct.Inl(value) => value.i
    case Coproduct.Inr(add)   => add.lhs + add.rhs

// And of course we'll need a `Functor`:
  given [Left[_]: Functor, Right[_]: Functor]: Functor[Left :+: Right] with
    extension [A](fa: (Left :+: Right)[A])
      def map[B](f: A => B) = fa match
        case Coproduct.Inl(value) => Coproduct.Inl(value.map(f))
        case Coproduct.Inr(value) => Coproduct.Inr(value.map(f))

  // But this is disappointing: if you pay attention, we've just copy / pasted the code for `valueEval` and `addEval`.
  // What we would _much_ prefer would be tell the compiler how to eval `Value` and `Add` separately, and then have it
  // work out how to do `Value :+: Add`. This is typically done through type classes, so let's write one:
  trait Eval[F[_]]:
    extension (fi: F[Int]) def eval: Int

  // Basic instances are trivial:
  given Eval[Add] with
    extension (fi: Add[Int]) def eval = fi.lhs + fi.rhs

  given Eval[Value] with
    extension (fi: Value[Int]) def eval = fi.i

  // Which allows us to quite easily derive the one for coproducts:
  given [Left[_]: Eval, Right[_]: Eval]: Eval[Left :+: Right] with
    extension (fi: (Left :+: Right)[Int])
      def eval = fi match
        case Coproduct.Inl(value) => value.eval
        case Coproduct.Inr(value) => value.eval

  // And now we can easily write our full evaluation function:
  def evaluate[F[_]: Eval: Functor](expr: Expr[F]): Int = expr.fold(_.eval)

  import Coproduct.*
  val expr: Expr[Value :+: Add] = Expr(Inr(Add(Expr(Inl(Value(118))), Expr(Inl(Value(1219))))))

  evaluate(expr)

// - Syntactic sugar ---------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** `expr` is painful for two reasons:
  *   - all the `Inl` and `Inr` layers to raise `Value` and `Add` in a `Value :+: Add`.
  *   - all the `Expr` to wrap things properly.
  */
object injection:
  case class Expr[F[_]](in: F[Expr[F]]):
    def fold[A](algebra: F[A] => A)(using Functor[F]): A =
      algebra(in.map(_.fold(algebra)))

  case class Add[A](lhs: A, rhs: A)
  case class Value[A](i: Int)

  enum Coproduct[Left[_], Right[_], A]:
    case Inl(value: Left[A])
    case Inr(value: Right[A])
  type :+:[Left[_], Right[_]] = [A] =>> Coproduct[Left, Right, A]
  import Coproduct.*

  // We could start by providing helpers to create values and additions.
  // Note that if we wanted to follow the paper more strictly, we'd start from `value` returning `Expr[Value]`, and
  // `add` returning `Expr[Add]`.
  def value(i: Int): Expr[(Value :+: Add)]                                         = Expr(Inl(Value(i)))
  def add(lhs: Expr[Value :+: Add], rhs: Expr[Value :+: Add]): Expr[Value :+: Add] = Expr(Inr(Add(lhs, rhs)))
  val expr: Expr[Value :+: Add]                                                    = add(value(118), value(1219))

// This is much more pleasant, but it rather defeats the purpose: we're now back to having this hard-coded to
// Value :+: Add. We want to have that for some `F[_]`.
// But it cannot be *any* F[_]: for `value`, `F` must be some coproduct that includes `Value`, for example.
// The idea for that is to be able to describe the relation _`F` can be injected in some `G`_. This feels like a good
// use case for type classes:
  trait :<:[Sub[_], Sup[_]]:
    self =>
    extension [A](fa: Sub[A]) def inject: Sup[A]

    // We can of course compose instances, which comes in handy when defining more complex ones:
    // This should be introduced later in a talk or article.
    def andThen[Sup2[_]](next: Sup :<: Sup2): (Sub :<: Sup2) = new (Sub :<: Sup2):
      extension [A](sa: Sub[A]) def inject = next.inject(self.inject(sa))

  // We can then give the obvious instances for it:
  // - reflexivity. A type can always be injected in itself.
  given reflexity[F[_]]: (F :<: F) with
    extension [A](fa: F[A]) def inject = fa

  // - transitivity. If F :<: G and G :<: H, then F :<: H.
  given transitivity[F[_], G[_], H[_]](using fg: F :<: G, gh: G :<: H): (F :<: H) = fg.andThen(gh)

  // - left injection: F can be injected in F :+: G
  given leftInject[F[_], G[_]]: (F :<: (F :+: G)) with
    extension [A](fa: F[A]) def inject = Inl(fa)

  // - right injection: F can be injected in G :+: F.
  given rightInject[F[_], G[_]]: (G :<: (F :+: G)) with
    extension [A](ga: G[A]) def inject = Inr(ga)

  // This appears to be necessary to appease the compiler when building more complex sets of operators.
  // If we have more than 2, that introduces an ambiguity: C :<: (A :+: B :+: C)
  given composeRight[F[_], G[_], H[_]](using fg: F :<: G): (F :<: (H :+: G)) = fg.andThen(rightInject)

  // This allows us to make `value` and `add` generic on the set of operators.
  def value2[F[_]](i: Int)(using Value :<: F): Expr[F]                 = Expr(Value(i).inject)
  def add2[F[_]](lhs: Expr[F], rhs: Expr[F])(using Add :<: F): Expr[F] = Expr(Add(lhs, rhs).inject)

  // For good measure, let's make sure this works:
  val expr2: Expr[Value :+: Add]  = add2(value2(118), value2(1219))
  val expr2b: Expr[Value :+: Add] = add2(add2(value2(112), value2(113)), value2(114))

  // If you're like me, you'll notice a common pattern which can be extracted:
  def inject[Sub[_], Sup[_]](value: Sub[Expr[Sup]])(using Sub :<: Sup): Expr[Sup] = Expr(value.inject)
  def value3[F[_]](i: Int)(using Value :<: F): Expr[F]                            = inject(Value(i))
  def add3[F[_]](lhs: Expr[F], rhs: Expr[F])(using Add :<: F): Expr[F]            = inject(Add(lhs, rhs))

  // And this still works:
  val expr3: Expr[Value :+: Add]  = add3(value3(118), value3(1219))
  val expr3b: Expr[Value :+: Add] = add3(add3(value3(112), value3(113)), value3(114))
// Note that for some reason I have not been able to get that to work with infix operators. Raise an issue with
// EPFL?

// - Final forms of coproducts, injections and expressions -------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object coproduct:
  enum Coproduct[Left[_], Right[_], A]:
    case Inl(value: Left[A])
    case Inr(value: Right[A])
  type :+:[Left[_], Right[_]] = [A] =>> Coproduct[Left, Right, A]

  given [Left[_]: Functor, Right[_]: Functor]: Functor[Left :+: Right] with
    extension [A](fa: (Left :+: Right)[A])
      def map[B](f: A => B) = fa match
        case Coproduct.Inl(value) => Coproduct.Inl(value.map(f))
        case Coproduct.Inr(value) => Coproduct.Inr(value.map(f))

object inj:
  import coproduct.*, Coproduct.*

  trait :<:[Sub[_], Sup[_]]:
    self =>
    extension [A](fa: Sub[A]) def inject: Sup[A]
    def andThen[Sup2[_]](next: Sup :<: Sup2): (Sub :<: Sup2) = new (Sub :<: Sup2):
      extension [A](sa: Sub[A]) def inject = next.inject(self.inject(sa))
  given reflexity[F[_]]: (F :<: F) with
    extension [A](fa: F[A]) def inject = fa
  given transitivity[F[_], G[_], H[_]](using fg: F :<: G, gh: G :<: H): (F :<: H) = fg.andThen(gh)
  given leftInject[F[_], G[_]]: (F :<: (F :+: G)) with
    extension [A](fa: F[A]) def inject = Inl(fa)
  def rightInject[F[_], G[_]]: (G :<: (F :+: G)) = new (G :<: (F :+: G)):
    extension [A](ga: G[A]) def inject = Inr(ga)
  given composeRight[F[_], G[_], H[_]](using fg: F :<: G): (F :<: (H :+: G)) = fg.andThen(rightInject)


object expr:
  import inj.*
  import coproduct.*, Coproduct.*

  case class Expr[F[_]](in: F[Expr[F]]):
    def fold[A](algebra: F[A] => A)(using Functor[F]): A =
      algebra(in.map(_.fold(algebra)))

  case class Add[A](lhs: A, rhs: A)
  case class Value[A](i: Int)

  def inject[Sub[_], Sup[_]](value: Sub[Expr[Sup]])(using Sub :<: Sup): Expr[Sup] = Expr(value.inject)
  def value[F[_]](i: Int)(using Value :<: F): Expr[F]                             = inject(Value(i))
  def add[F[_]](lhs: Expr[F], rhs: Expr[F])(using Add :<: F): Expr[F]             = inject(Add(lhs, rhs))

  given Functor[Value] with
    extension [A](fa: Value[A]) def map[B](f: A => B) = Value(fa.i)
  given Functor[Add] with
    extension [A](fa: Add[A]) def map[B](f: A => B) = Add(f(fa.lhs), f(fa.rhs))

  trait Eval[F[_]]:
    extension (fi: F[Int]) def eval: Int

  // Basic instances are trivial:
  given Eval[Add] with
    extension (fi: Add[Int]) def eval = fi.lhs + fi.rhs

  given Eval[Value] with
    extension (fi: Value[Int]) def eval = fi.i

  // Which allows us to quite easily derive the one for coproducts:
  given [Left[_]: Eval, Right[_]: Eval]: Eval[Left :+: Right] with
    extension (fi: (Left :+: Right)[Int])
      def eval = fi match
        case Coproduct.Inl(value) => value.eval
        case Coproduct.Inr(value) => value.eval

  // And now we can easily write our full evaluation function:
  def evaluate[F[_]: Eval: Functor](expr: Expr[F]): Int = expr.fold(_.eval)

// - Solving the expression problem ------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Let's now try to add a new operator: multiplication.
  *
  * This is simply a matter of writing the boilerplate - a `Functor` instance, an `Eval` instance, and the helper
  * function.
  */
object multiplication:
  import expr.*,  inj.*, coproduct.*

  case class Mult[A](lhs: A, rhs: A)

  given Functor[Mult] with
    extension [A](fa: Mult[A]) def map[B](f: A => B) = Mult(f(fa.lhs), f(fa.rhs))

  given Eval[Mult] with
    extension (fi: Mult[Int]) def eval = fi.lhs * fi.rhs

  def mult[F[_]](lhs: Expr[F], rhs: Expr[F])(using Mult :<: F): Expr[F] = inject(Mult(lhs, rhs))

  // And we can no easily create expressions that contain multiplications.
  val multExpr: Expr[Value :+: Add :+: Mult] = mult(add(value(112), value(113)), value(114))
  val multExpr2: Expr[Mult :+: Value]        = mult(value(2), value(3))

/** And here's a new interpreter.
  *
  * Let's do it the easy way first, which is not the way the paper does it. We'll just duplicate what we did for `Eval`,
  * which makes it trivially easy.
  */
object rendr1:
  import expr.*,  multiplication.*,  inj.*,  coproduct.*

  // A simple pretty printer.
  trait Render[F[_]]:
    extension (fs: F[String]) def render: String

  // Instances of it.
  given Render[Mult] with
    extension (mult: Mult[String]) def render = s"${mult.lhs} * ${mult.rhs}"

  given Render[Add] with
    extension (add: Add[String]) def render = s"${add.lhs} + ${add.rhs}"

  given Render[Value] with
    extension (value: Value[String]) def render = value.toString

  given [Left[_]: Render, Right[_]: Render]: Render[Left :+: Right] with
    extension (fi: (Left :+: Right)[String])
      def render = fi match
        case Coproduct.Inl(value) => value.render
        case Coproduct.Inr(value) => value.render

  // Pretty printing an expression is then merely a fold.
  def render[F[_]: Render: Functor](expr: Expr[F]): String = expr.fold(_.render)

/** The paper decided to take another, slightly more complicated approach.
  *
  * This was, I think, for (questionable!) pedagogical reasons. Interpreters that work via fold are necessarily
  * "complete" and ordered: you cannot short-circuit them, or change the order of evaluation. In order to be able to do
  * that, you'd need to have access to the next step in the recursion, which `fold`s don't grant.
  */
object rendr2:
  import expr.*, multiplication.*, expr.*, coproduct.*

  // We have access to the next step in the recursion here: the `Expr[F]`.
  trait RenderV1[F[_]]:
    extension (fs: F[Expr[F]]) def render: String

  // There's an obvious problem, however.
  // We've lost all flexibility. We can no longer render composite operators, since `Add` can only contain `Add`
  // (which is problematic, because we can't even create a value of that type!).
  given RenderV1[Add] with
    extension (fs: Add[Expr[Add]]) def render = ???

  // Instead, we can define `Render` by leaving the next step free, with the sole constrain that we know how
  // to render it. The recursion is now open: `F[Expr[G]]` is non recursive, but nothing says that `G` can't be the
  // same type as `F`.
  trait Render[F[_]]:
    extension [G[_]: Render](fs: F[Expr[G]]) def render: String

  // And now `Render[Add]` works for any nested type.
  // I'm a little unsure what the point is _in the context of this paper_ though. You can't know anything about `G`,
  // so there's no interesting thing you can do such as "only go down if it's an `Add`, otherwise ignore".
  //
  // We could imagine having nodes in the AST only present there to help the compiler, say, but not needing to be
  // represented.
  given Render[Add] with
    extension [G[_]: Render](add: Add[Expr[G]]) def render = s"${add.lhs} + ${add.rhs}"

  given Render[Mult] with
    extension [G[_]: Render](mult: Mult[Expr[G]]) def render = s"${mult.lhs} * ${mult.rhs}"

  given Render[Value] with
    extension [G[_]: Render](value: Value[Expr[G]]) def render = value.toString

  // Of course, we need to be able to render coproducts. It's trivial boilerplate, but we need to write it.
  given [Left[_]: Render, Right[_]: Render]: Render[Left :+: Right] with
    extension [G[_]: Render](fi: (Left :+: Right)[Expr[G]])
      def render = fi match
        case Coproduct.Inl(value) => value.render
        case Coproduct.Inr(value) => value.render

  // Rendering is now trivial, since we're not folding any longer. Just allowing the compiler to work out the
  // chain of type class instances we need.
  def render[F[_]: Render](expr: Expr[F]): String = expr.in.render

  render(multExpr)

// The paper then realises that `:<:` is very much a higher kinded prism, and explores this for a bit.
object aside:
  import expr.*, coproduct.*, Coproduct.*, multiplication.*

  // If we add the `project` method, we can also project supertypes into subtypes - sometimes.
  trait :<:[Sub[_], Sup[_]]:
    self =>
    extension [A](fa: Sub[A]) def inject: Sup[A]
    extension [A](fa: Sup[A]) def project: Option[Sub[A]]

  // We need to rewrite these because `:<:` changed, but they add nothing new.
  def inject[Sub[_], Sup[_]](value: Sub[Expr[Sup]])(using Sub :<: Sup): Expr[Sup] = Expr(value.inject)
  def value[F[_]](i: Int)(using Value :<: F): Expr[F]                             = inject(Value(i))
  def add[F[_]](lhs: Expr[F], rhs: Expr[F])(using Add :<: F): Expr[F]             = inject(Add(lhs, rhs))
  def mult[F[_]](lhs: Expr[F], rhs: Expr[F])(using Mult :<: F): Expr[F]           = inject(Mult(lhs, rhs))

  // This allows us to write a specialised `as` method that attempts to turn an expression into a specialised one.
  extension [F[_]](expr: Expr[F])
    def as[G[_]](using G :<: F): Option[G[Expr[F]]] =
      expr.in.project

  // What would be the point? Well, we could use that to rewrite expressions of shape a * (b + c) to (a * b) + (a * c)
  def distr[F[_]](expr: Expr[F])(using Mult :<: F, Add :<: F): Option[Expr[F]] = for
    Mult(a, rhs) <- expr.as[Mult]
    Add(b, c)    <- rhs.as[Add]
  yield mult(add(a, b), add(a, c))

  // I would personally have gone a little further and made that recursive, and return a non-option type, just to show
  // it could be done on arbitrarily complex expressions.

// - Free monads -------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/** The paper then makes the argument that this idea can be generalised to the Free monad. I'm not going to explain
  * `Free`, mostly because I've done so in a rather long article.
  */
object term:
  import inj.*, coproduct.*

  // This is Free.
  enum Term[F[_], A]:
    case Pure(value: A)
    case Impure(in: F[Term[F, A]])

    // Folding is a little more complicated now: we need a case for the pure and impure variants.
    // The impure is exactly what we had for Expr, and the pure is easy.
    def fold[B](pure: A => B, impure: F[B] => B)(using Functor[F]): B = this match
      case Pure(a)    => pure(a)
      case Impure(fb) => impure(fb.map(_.fold(pure, impure)))

  // `inject` is very much the same as what we wrote for `Fix` - or, rather, `Expr`.
  def inject[Sub[_], Sup[_], A](value: Sub[Term[Sup, A]])(using Sub :<: Sup): Term[Sup, A] = Term.Impure(value.inject)

  // Monad instance of `Free` for any functor `F`.
  given [F[_]: Functor]: Monad[Term[F, _]] with
    extension [A](fa: Term[F, A])
      def map[B](f: A => B) = fa match
        case Term.Pure(a)    => Term.Pure(f(a))
        case Term.Impure(ft) => Term.Impure(ft.map(_.map(f)))

    extension [A](a: A) def pure = Term.Pure(a)
    extension [A, B](fa: Term[F, A])
      def flatMap(f: A => Term[F, B]) = fa match
        case Term.Pure(a)    => f(a)
        case Term.Impure(ft) => Term.Impure(ft.map(_.flatMap(f)))

  // Simple example of monad expressable as Free: ID
  opaque type Zero[A] = Nothing
  given Functor[Zero] with
    extension [A](fa: Zero[A]) def map[B](f: A => B) = ???
  // This is the same as Id: since we can't produce a value of type Zero[A] for any A, then the only legal value
  // of type Term[Zero, A] is Pure(a) - so the value itself.
  type Id[A] = Term[Zero, A]

  // Another one: Option
  opaque type One[A] = Unit
  given Functor[One] with
    extension [A](fa: One[A]) def map[B](f: A => B) = ()
  // This is Maybe: we have two possible values, Pure(a) or Impure(()). So, either A or not.
  type Maybe[A] = Term[One, A]

  opaque type Const[E, A] = E
  given [E]: Functor[Const[E, _]] with
    extension [A](fa: Const[E, A]) def map[B](f: A => B) = fa
  // The only values are Pure(a) or Impure(e). It's Either.
  type Error[E, A] = Term[Const[E, _], A]

/** A "simple" application of using `Term` to model composable computations: a simple calculator.
  *
  * We can either increment the content of a memory cell, or recall it.
  *
  * Note that since we're using `Term`, which is really `Free`, the things that go in it must be modelled with explicit
  * continuations.
  */
object calculator:
  import term.*, coproduct.*, inj.*

  // Increments the content of the memory cell by `i`.
  case class Incr[A](i: Int, next: A)
  given Functor[Incr] with
    extension [A](fa: Incr[A]) def map[B](f: A => B) = fa.copy(next = f(fa.next))
  def incr[F[_]](i: Int)(using Incr :<: F): Term[F, Unit] = inject(Incr(i, Term.Pure(())))

  // Passes the content of the memory cell to the next step.
  case class Recall[A](next: Int => A)
  given Functor[Recall] with
    extension [A](fa: Recall[A]) def map[B](f: A => B) = Recall(fa.next andThen f)
  def recall[F[_]](using Recall :<: F): Term[F, Int]      = inject(Recall(i => Term.Pure(i)))

  // And here's a simple program that increments the content of the memory cell by 1, and returns the previous
  // content.
  // Since `Term[Incr :+: Recall, _]` is monadic, we can use for comprehensions. But Scala type inference being what it
  // is, we need lots of explicit annotations...
  val tick: Term[Incr :+: Recall, Int] = for
    y <- recall[Incr :+: Recall]
    _ <- incr[Incr :+: Recall](1)
  yield y

  // We're now going to try and evaluate that simple program. Since it has state - the content of the memory cell -
  // our evaluator will need to take the initial state. Let's give that a type:
  case class Mem(value: Int)

  // Compiling a term yields a function that, given a `Mem`, returns the result of the computation and the new state
  // of the memory. We'll give that a type.
  type Runner[A] = Mem => (A, Mem)

  // Run - which should probably be called `Compile` - is exactly what we need for a fold that produces a runner:
  // the algebra F[Target] => Target, that is, F[Runner[A]] => Runner[A].
  trait Run[F[_]]:
    extension [A](fa: F[Runner[A]]) def run: Runner[A]

  given Run[Incr] with
    extension [A](fa: Incr[Runner[A]]) def run = mem => fa.next(Mem(mem.value + fa.i))

  given Run[Recall] with
    extension [A](fa: Recall[Runner[A]]) def run = mem => fa.next(mem.value)(mem)

  given [F[_]: Run, G[_]: Run]: Run[F :+: G] with
    extension [A](fa: (F :+: G)[Runner[A]])
      def run = fa match
        case Coproduct.Inl(fa) => fa.run
        case Coproduct.Inr(ga) => ga.run

  def run[F[_]: Run: Functor, A](term: Term[F, A], mem: Mem): (A, Mem) =
    // Compilation is easy, even if the types are a little hard to follow. I've annotated them for convenience.
    val runner: Runner[A] = term.fold(
      pure = a => mem => (a, mem), // A => Runner[A]
      impure = _.run               // F[Runner[A]] => Runner[A]
    )

    runner(mem)

// - Effect tracking ---------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Finally, we can use this to capture effects. Here, for example, we'll use the "works with files" and "works with the
  * terminal" effects together.
  */
object io:
  import term.*, coproduct.*, inj.*

  // Terminal: we can read a line from it, or print a line to it.
  enum Teletype[A]:
    case ReadLine(next: String => A)
    case PrintLine(line: String, next: A)

  // smart constructors to lift `Teletype` in `Term`.
  def readLine[F[_]](using Teletype :<: F): Term[F, String] = inject(Teletype.ReadLine(Term.Pure.apply))
  def printLine[F[_]](line: String)(using Teletype :<: F): Term[F, Unit] = inject(
    Teletype.PrintLine(line, Term.Pure(()))
  )

  given Functor[Teletype] with
    extension [A](fa: Teletype[A])
      def map[B](f: A => B) = fa match
        case Teletype.ReadLine(next)        => Teletype.ReadLine(next andThen f)
        case Teletype.PrintLine(char, next) => Teletype.PrintLine(char, f(next))

  // File operations: we can write content to a file, or read content from a file.
  enum FileSystem[A]:
    case ReadFile(path: File, next: String => A)
    case WriteFile(path: File, content: String, next: A)

  // Smart constructors to lift `FileSystem` into `Term`.
  def readFile[F[_]](path: File)(using FileSystem :<: F): Term[F, String] = inject(
    FileSystem.ReadFile(path, s => Term.Pure(s))
  )
  def writeFile[F[_]](path: File, content: String)(using FileSystem :<: F): Term[F, Unit] = inject(
    FileSystem.WriteFile(path, content, Term.Pure(()))
  )

  given Functor[FileSystem] with
    extension [A](fa: FileSystem[A])
      def map[B](f: A => B) = fa match
        case FileSystem.ReadFile(path, next)           => FileSystem.ReadFile(path, next andThen f)
        case FileSystem.WriteFile(path, content, next) => FileSystem.WriteFile(path, content, f(next))

  // A simple program that reads the content of a file and prints it to the terminal.
  // Again, type inference isn't great in for-comprehensions, for reasons that currently escape me, so we need to
  // type annotate everything.
  // Also, we could probably split this further in to read from term, write to term, read from file, write to file...
  def cat(path: File): Term[Teletype :+: FileSystem, Unit] = for
    content <- readFile[Teletype :+: FileSystem](path)
    _       <- printLine[Teletype :+: FileSystem](content)
  yield ()

  // And of course we now need to be able to execute that program. We'll do the same thing as for `calc`, where we
  // compile it to something executable: an `IO`.
  // We know we want our return type to be `IO[A]`, so, given how folds work, we need some `F[IO[A]] => IO[A]` - the
  // F-algebra of our operators.
  trait Exec[F[_]]:
    extension [A](fa: F[IO[A]]) def exec: IO[A]

  // Trival instance for coproducts.
  given [F[_]: Exec, G[_]: Exec]: Exec[F :+: G] with
    extension [A](fa: (F :+: G)[IO[A]])
      def exec: IO[A] = fa match
        case Coproduct.Inl(fa) => fa.exec
        case Coproduct.Inr(ga) => ga.exec

  // The instances for `Teletype` and `FileSystem` are really just flatMapping into the continuation.
  given Exec[Teletype] with
    extension [A](fa: Teletype[IO[A]])
      def exec: IO[A] = fa match
        case Teletype.ReadLine(next)        => Console[IO].readLine.flatMap(next)
        case Teletype.PrintLine(line, next) => Console[IO].println(line).flatMap(_ => next)

  given Exec[FileSystem] with
    extension [A](fa: FileSystem[IO[A]])
      def exec: IO[A] = fa match
        case FileSystem.ReadFile(path, next) => IO(scala.io.Source.fromFile(path).getLines.mkString("\n")).flatMap(next)
        case FileSystem.WriteFile(path, content, next) =>
          IO(Files.write(path.toPath(), content.getBytes("UTF-8"))).flatMap(_ => next)

  // And execution is now very simple, a simple fold:
  def exec[F[_]: Exec: Functor, A](fa: Term[F, A]): IO[A] = fa.fold(
    pure = IO.pure,
    impure = _.exec
  )

  val runCat =
    val prg = for
      _ <- printLine[Teletype :+: FileSystem]("About to print ./build.sbt...")
      _ <- cat(new File("./build.sbt"))
    yield ()
    exec(prg)

@main def runTerm =
  import cats.effect.unsafe.implicits.global
  io.runCat.unsafeRunSync()
