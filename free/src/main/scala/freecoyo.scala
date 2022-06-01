package free.coyoneda

import yoneda.CoYoneda
import free.monad.Free
import free.monad.given
import abstractions.*

// A console can be used to do 3 things: print to stdout, read from stdin, and quit.
enum Console[+A]:
  case Print(value: String) extends Console[Unit]
  case Read extends Console[String]
  case Stop extends Console[Unit]

type FreeConsole[A] = Free[CoYoneda[Console, _], A]

object FreeConsole:
  def print(str: String): FreeConsole[Unit] = Free.lift(CoYoneda.lift(Console.Print(str)))
  def read: FreeConsole[String]             = Free.lift(CoYoneda.lift(Console.Read))
  def stop: FreeConsole[Unit]               = Free.lift(CoYoneda.lift(Console.Stop))

def toMonad[F[_], G[_]: Monad, A](fca: Free[CoYoneda[F, _], A])(f: F ~> G): G[A] =
  fca.toMonad(
    new (CoYoneda[F, _] ~> G):
      override def apply[A](fa: CoYoneda[F, A]): G[A] =
        f(fa.source).map(fa.transformation)
  )

@main def askName =
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
      case Console.Print(value) =>
        println(value)
        Some(())

      case Console.Read =>
        Some(scala.io.StdIn.readLine)

      case Console.Stop => None

  val result = toMonad(ask)(handler)

  println(s"Returned value: $result")
