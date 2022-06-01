package free.monad

import abstractions.*

// - Free --------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// The free monad over a Functor: if F has an instance of Functor, then Free has an instance of Monad.
enum Free[F[_], A]:
  case Pure(a: A)
  case Impure(value: F[Free[F, A]])

  // Effectful interpretation: if we know how to turn our F into some monad G, then
  // we know how to turn a Free[F, A] into a G[A].
  def toMonad[G[_]: Monad](transformation: F ~> G)(using Functor[F]): G[A] = this match
    case Pure(a)       => a.pure
    case Impure(value) => transformation(value).flatMap(_.toMonad(transformation))

  // If we know how to turn a program into its normal form A, then we can turn Free[F, A] into an A.
  def run(handler: F[A] => A)(using Functor[F]): A = this match
    case Pure(a) => a
    case Impure(value) =>
      handler(value.map(_.run(handler)))

object Free:
  def lift[F[_]: Functor, A](fa: F[A]): Free[F, A] = Free.Impure(fa.map(Free.Pure.apply))

given [F[_]: Functor]: Monad[Free[F, _]] with
  extension [A](a: A) def pure = Free.Pure(a)

  extension [A](ffa: Free[F, A])
    def map[B](f: A => B) = ffa match
      case Free.Pure(a)       => Free.Pure(f(a))
      case Free.Impure(value) => Free.Impure(value.map(_.map(f)))

    def flatMap[B](f: A => Free[F, B]) = ffa match
      case Free.Pure(a)       => f(a)
      case Free.Impure(value) => Free.Impure(value.map(_.flatMap(f)))

// - Console -----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// A console can be used to do 3 things: print to stdout, read from stdin, and quit.
enum Console[+A]:
  case Print(value: String, next: () => A)
  case Read(continuation: String => A)
  case Stop

given Functor[Console] with
  extension [A](fa: Console[A])
    def map[B](f: A => B) = fa match
      case Console.Print(value, continuation) => Console.Print(value, () => f(continuation()))
      case Console.Read(continuation)         => Console.Read(input => f(continuation(input)))
      case Console.Stop                       => Console.Stop

type FreeConsole[A] = Free[Console, A]

object FreeConsole:
  def print(str: String): FreeConsole[Unit] = Free.lift(Console.Print(str, () => ()))
  def read: FreeConsole[String]             = Free.lift(Console.Read(identity))
  def stop: FreeConsole[Unit]               = Free.lift(Console.Stop)

// - Test --------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

@main def freeAskName =
  // Simple program that asks the user for his name and:
  // - crashes if that name is Nicolas
  // - greets everybody else, and exits with their name as the return value.
  val ask: FreeConsole[String] = for
    _    <- FreeConsole.print("What is your name?")
    name <- FreeConsole.read
    _    <- if(name == "Nicolas") FreeConsole.stop else FreeConsole.print(s"Greetings, ${name}")
  yield name

  val handler = new (Console ~> Option):
    override def apply[A](fa: Console[A]) = fa match
      case Console.Print(value, next) =>
        println(value)
        Some(next())

      case Console.Read(next) =>
        Some(next(scala.io.StdIn.readLine))

      case Console.Stop => None

  val result = ask.toMonad(handler)
  println(s"Returned value: $result")

@main def freePrompt =
  val handler: (Console[Unit] => Unit) =
    case Console.Print(value, next) =>
      println(value)
      next()

    case Console.Read(next) =>
      next(scala.io.StdIn.readLine)

    case Console.Stop => ()

  def runCommand(cmd: String): FreeConsole[Unit] = cmd match
    case "stop" => FreeConsole.stop
    case "help" =>
      for
        _ <- FreeConsole.print("'help' for help message, 'stop' to quit")
        _ <- prompt
      yield ()
    case other =>
      for
        _ <- FreeConsole.print(s"Unknown command: $other")
        _ <- prompt
      yield ()

  def prompt: FreeConsole[Unit] = for
    _   <- FreeConsole.print("Type command (help for usage):")
    cmd <- FreeConsole.read
    _   <- runCommand(cmd)
  yield ()

  prompt.run(handler)
