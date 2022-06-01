package freer

import abstractions.*
import scala.annotation.tailrec

// - Freer -------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// The free monad over any type constructor - a simple defunctionalisation of pure and flatMap.
enum Freer[F[_], A]:
  case Pure(a: A)
  case FlatMap[F[_], X, A](value: F[X], cont: X => Freer[F, A]) extends Freer[F, A]

  // Interprets our F into a G using the specified natural transformation.
  def toMonad[G[_]: Monad](f: F ~> G): G[A] = this match
    case Pure(a)              => a.pure
    case FlatMap(value, cont) => f(value).flatMap(x => cont(x).toMonad(f))

// Interprets our F into its normal form using the specified handler.
  def run(handler: Freer.Handler[F, A]): A = this match
    case Pure(a) => a
    case FlatMap(value, cont) =>
      val next = cont andThen (_.run(handler))

      handler(next, value)

object Freer:
  // Lifts an F into Freer.
  def lift[F[_], A](fa: F[A]): Freer[F, A] = Freer.FlatMap(fa, Freer.Pure.apply)

  // How to interpret an F whose normal form is B into a B.
  trait Handler[F[_], B]:
    def apply[A](resume: A => B, fa: F[A]): B

// Monad instance of Freer. Note how there is absolutely no constraint on F.
given [F[_]]: Monad[Freer[F, _]] with
  extension [A](a: A) def pure = Freer.Pure(a)

  extension [A](fa: Freer[F, A])
    def map[B](f: A => B) = fa match
      case Freer.Pure(a)              => Freer.Pure(f(a))
      case Freer.FlatMap(state, cont) => Freer.FlatMap(state, a => cont(a).map(f))

    def flatMap[B](f: A => Freer[F, B]) = fa match
      case Freer.Pure(a)              => f(a)
      case Freer.FlatMap(state, cont) => Freer.FlatMap(state, a => cont(a).flatMap(f))

// - Console -----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Basic instructions for a console that can read from stdin, print to stdout or quit.
enum Console[A]:
  case Print(value: String) extends Console[Unit]
  case Read extends Console[String]
  case Quit extends Console[Unit]

// Console, lifted into Freer
type ConsoleF[A] = Freer[Console, A]

// Helper functions, makes creating Console programs a little less painful.
object ConsoleF:
  def print(value: String): ConsoleF[Unit] = Freer.lift(Console.Print(value))
  val read: ConsoleF[String]               = Freer.lift(Console.Read)
  val quit: ConsoleF[Unit]                 = Freer.lift(Console.Quit)

// - Direct interpreter ------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Demonstrates how to write a direct interpreter.
// Since we know the normal form (Unit), it's possible to interrupt the flow of the program when encountering
// Stop and just return ().
@main def freerPrompt =
  def runCommand(cmd: String): ConsoleF[Unit] = cmd match
    case "stop" => ConsoleF.quit
    case "help" =>
      for
        _ <- ConsoleF.print("'help' for help message, 'stop' to quit")
        _ <- prompt
      yield ()
    case other =>
      for
        _ <- ConsoleF.print(s"Unknown command: $other")
        _ <- prompt
      yield ()

  def prompt: ConsoleF[Unit] = for
    _   <- ConsoleF.print("Type command (help for usage):")
    cmd <- ConsoleF.read
    _   <- runCommand(cmd)
  yield ()

  val handler = new Freer.Handler[Console, Unit]:
    def apply[A](resume: A => Unit, current: Console[A]): Unit = current match
      case Console.Print(value) =>
        println(value)
        resume(())

      case Console.Read =>
        resume(scala.io.StdIn.readLine)

      case Console.Quit =>
        ()

  prompt.run(handler)

// - Natural Transformation --------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// "Effectful" interpreter that doesn't yield a program's normal form, but its normal form within some monad F.
// This makes handling Quit a little less pleasant, as we must use types with a default value (such as Option with None)
// to represent non-termination.
@main def freerAskName =
  val ask: ConsoleF[String] = for
    _    <- ConsoleF.print("What is your name?")
    name <- ConsoleF.read
    _    <- if(name == "Nicolas") ConsoleF.quit else ConsoleF.print(s"Greetings, ${name}")
  yield name

  val handler = new (Console ~> Option):
    override def apply[A](ca: Console[A]) = ca match
      case Console.Print(value) =>
        Some(println(value))

      case Console.Read =>
        Some(scala.io.StdIn.readLine)

      case Console.Quit => None

  val result = ask.toMonad(handler)
  println(s"Returned value: $result")

@main def freerStackSafe =
  def ask(i: Int): ConsoleF[Unit] = for
    _ <- ConsoleF.print(s"Loop $i")
    _ <- ask(i + 1)
  yield ()

  val handler = new Freer.Handler[Console, Unit]:
    def apply[A](resume: A => Unit, current: Console[A]): Unit = current match
      case Console.Print(value) =>
        println(value)
        resume(())

      case Console.Read =>
        resume(scala.io.StdIn.readLine)

      case Console.Quit =>
        ()

  ask(1).run(handler)
