// I was recently told to look at https://www.cs.utexas.edu/~wcook/Drafts/2012/ecoop2012.pdf as it might be of
// interest to something I'm currently looking into (sum types, and how to deal with their absence).
//
// After playing a little bit with the paper, I realised that it was basically Tagless Final, as described in
// https://okmij.org/ftp/tagless-final/course/lecture.pdf.
// The only difference I could really find was the verbosity caused by the absence of type classes.
//
// This bit of code was just to confirm that I could define distinct DSLs, and combine them without needing to modify
// them in any way - basically solve the expression problem.
// I do not think I'm using anything that couldn't be done in Java here, except using traits to allow me to create
// compound algebras by subclassing multiple algebras. This can be worked around relatively easily, at the cost of
// additional verbosity.

// - Misc. -------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Simple type aliases we'll need later.
type PrettyPrinted[_] = String
type Id[A]            = A

// - Ints DSL ----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// A DSL is represented by its syntax, which in the context of object algebras is called an algebra, and Tagless Final
// calls symantic. This is an abstract class (a trait here, to allow multiple inheritance).
//
// Values

// Algebra for integer operations.
// Not that it's parameterized on a type constructor because I'm cheating: I know that I'll eventually need to compose
// this with another DSL that handles booleans, and that I will want to tag expressions with their return types
// in order to avoid nonsensical statements such as 1 + (2 == 3).
trait IntAlgebra[F[_]]:
  def num(value: Int): F[Int]
  def add(lhs: F[Int], rhs: F[Int]): F[Int]

// Pretty-printing of integer operations.
// We're declaring this as a trait to allow other algebras, such as IntBoolPrettyPrint, to easily reuse it
// by simple subclassing.
trait IntPrettyPrint extends IntAlgebra[PrettyPrinted]:
  override def num(value: Int)                       = value.toString
  override def add(lhs: String, rhs: String): String = s"$lhs + $rhs"

// Convenience, this gives us an algebra as a value, rather than having to instanciate IntPrettyPrint every time.
object IntPrettyPrint extends IntPrettyPrint

// Evaluator for our integer-based DSL.
trait IntEval extends IntAlgebra[Id]:
  override def num(value: Int)         = value
  override def add(lhs: Int, rhs: Int) = lhs + rhs

object IntEvasl extends IntEval

// - Booleans DSL ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Exactly the same thing, but for simple boolean expressions.

trait BoolAlgebra[F[_]]:
  def iff[A](test: F[Boolean], ifCase: F[A], elseCase: F[A]): F[A]
  def bool(value: Boolean): F[Boolean]

trait BoolPrettyPrint extends BoolAlgebra[PrettyPrinted]:
  override def iff[A](test: String, ifCase: String, elseCase: String) =
    s"if $test then $ifCase else $elseCase"

  override def bool(value: Boolean) = value.toString

object BoolPrettyPrint extends BoolPrettyPrint

trait BoolEval extends BoolAlgebra[Id]:
  override def iff[A](test: Boolean, ifCase: A, elseCase: A) =
    if test then ifCase
    else elseCase

  override def bool(value: Boolean) = value

object BoolEval extends BoolEval

// - Combined ----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Create an entirely new DSL by combining operations on booleans, integers, and providing a bridge between the two:
// equality checking, which might take two integer-based statements and return a boolean-based one.

trait IntBoolAlgebra[F[_]] extends IntAlgebra[F] with BoolAlgebra[F]:
  def eq[A](lhs: F[A], rhs: F[A]): F[Boolean]

trait IntBoolPrettyPrint extends IntBoolAlgebra[PrettyPrinted] with IntPrettyPrint with BoolPrettyPrint:
  override def eq[A](lhs: String, rhs: String) = s"$lhs == $rhs"

object IntBoolPrettyPrint extends IntBoolPrettyPrint

trait IntBoolEval extends IntBoolAlgebra[Id] with IntEval with BoolEval:
  override def eq[A](lhs: A, rhs: A) = lhs == rhs

object IntBoolEval extends IntBoolEval

// - DSL expressions as values -----------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Expressions of our DSL can be represented as something that is basically a function from an algebra to whatever
// that algebra evaluates to.
trait IntBoolExpr[A]:
  def accept[F[_]](algebra: IntBoolAlgebra[F]): F[A]

// Algebra used to create values of type IntBoolExpr.
// It's a little bit meta, as it's an algebra that builds values that defer to another algebra.
trait IntBoolFactory extends IntBoolAlgebra[IntBoolExpr]:
  override def num(value: Int) = new IntBoolExpr[Int]:
    def accept[F[_]](algebra: IntBoolAlgebra[F]) = algebra.num(value)

  override def add(lhs: IntBoolExpr[Int], rhs: IntBoolExpr[Int]) = new IntBoolExpr[Int]:
    def accept[F[_]](algebra: IntBoolAlgebra[F]) = algebra.add(lhs.accept(algebra), rhs.accept(algebra))

  override def iff[A](test: IntBoolExpr[Boolean], ifCase: IntBoolExpr[A], elseCase: IntBoolExpr[A]) =
    new IntBoolExpr[A]:
      def accept[F[_]](algebra: IntBoolAlgebra[F]) =
        algebra.iff(test.accept(algebra), ifCase.accept(algebra), elseCase.accept(algebra))

  override def bool(value: Boolean) = new IntBoolExpr[Boolean]:
    def accept[F[_]](algebra: IntBoolAlgebra[F]) = algebra.bool(value)

  override def eq[A](lhs: IntBoolExpr[A], rhs: IntBoolExpr[A]) = new IntBoolExpr[Boolean]:
    def accept[F[_]](algebra: IntBoolAlgebra[F]) = algebra.eq(lhs.accept(algebra), rhs.accept(algebra))

object IntBoolFactory extends IntBoolFactory

// - Tests -------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
@main def main =
  import IntBoolFactory.*

  // if 1 == 2 then 1 else 2 + 3
  val exp = iff(eq(num(1), num(2)), num(1), add(num(2), num(3)))

  println(exp.accept(IntBoolPrettyPrint))
  print("> ")
  println(exp.accept(IntBoolEval))
