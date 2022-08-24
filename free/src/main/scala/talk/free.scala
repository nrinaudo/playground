package talk.free

import abstractions.*

object v1:
  // Tried but true motivating example: a simple console program, which can print something to stdout,
  // read something from stdin, or quit.
  // We want to represent that as basic instructions, to provide various interpreters.
  enum Console:
    case Print(value: String)
    case Read
    case Stop

  // We quickly run into a problem, though: we do not know what the various instructions evaluate to.
  def eval(statement: Console): Any = statement match
    case Console.Print(value) => println(value)
    case Console.Read         => scala.io.StdIn.readLine
    case Console.Stop         => ()

object v2:
  // Solution: encode this information into each branch of our ADT. A is an instruction's normal form.
  enum Console[A]:
    case Print(value: String) extends Console[Unit]
    case Read extends Console[String]
    case Stop extends Console[Unit]

  // This allows us to write an interpreter with a sane return type.
  def eval[A](statement: Console[A]): A = statement match
    case Console.Print(value) => println(value)
    case Console.Read         => scala.io.StdIn.readLine
    case Console.Stop         => ()

  // Writing sequences of instructions is, however, not good enough: there's no way to pass a value from one
  // step to the next. For example, we cannot use the outcome of Console.Read in following steps.
  val prog = List(
    Console.Print("What is your name?"),
    Console.Read,
    Console.Print("Hello!"),
    Console.Stop
  )

object v3:
  // The problem is that our GADT describes the structure of a statement, not of a program. We can fix that by
  // having each step link to the next one.
  enum Console[+A]:
    case Print(value: String, next: Console[A])
    case Read(next: String => Console[A])
    case Stop

  val prog: Console[Unit] =
    Console.Print("What is your name?", Console.Read(name => Console.Print(s"Hello, $name!", Console.Stop)))

// The problem here is that we must terminate every single one of our programs with Console.Stop. It's not possible
// any longer to create partial programs - such as one to retrieve the user's name composed with another to greet
// a user, given a user name.

object v4:
  // In order to work around this, we must relax the type of our continuations (our next steps): We must allow them to
  // be something else than a Console.
  enum Console[+A]:
    case Print(value: String, next: A)
    case Read(next: String => A)
    case Stop

  // This works, but our programs have rather a nasty type now...
  val ask: Console[Console[String]] = Console.Print("What is your name?", Console.Read(identity))

object v5:
  // Recall our Console type.
  enum Console[+A]:
    case Print(value: String, next: A)
    case Read(next: String => A)
    case Stop

  // When presented with recursive types like this - Console[Console[Console[...]]] - one should always think of Fix
  // as a way to get back to sanity.
  case class Fix[F[_]](unwrap: F[Fix[F]])

  type ConsoleF = Fix[Console]
  // Of course, this only adds more problems, as Fix tends to do:
  // - we've lost the normal form of our program (ConsoleF takes no type parameter)
  // - we've just frozen the types of our continuations to ConsoleF, so we're back where we started..
  val prog: ConsoleF = Fix(
    Console.Print(
      "What is your name?",
      Fix(Console.Read(name => Fix(Console.Print(s"Hello, $name!", Fix(Console.Stop)))))
    )
  )

object v5bis:
  // Recall our Console type.
  enum Console[+A]:
    case Print(value: String, next: A)
    case Read(next: String => A)
    case Stop

  // We can update Fix to contain the normal form of our program:
  case class Fix2[F[_], A](unwrap: F[Fix2[F, A]])

  type ConsoleF[A] = Fix2[Console, A]
  val prog: ConsoleF[Unit] = Fix2(
    Console.Print(
      "What is your name?",
      Fix2(Console.Read(name => Fix2(Console.Print(s"Hello, $name!", Fix2(Console.Stop)))))
    )
  )
// This works, we've gotten our output type back. We must now deal with the fact that we cannot interrupt our
// program with anything but Console.Stop anymore

object v6:
  // We need a way to inject values that are not a console (because Console is either Stop or needs a continuation).
  // Fix2 needs to provide a non-recursive, value alternative.
  enum Fix2[F[_], A]:
    case Value(a: A)
    case Program(unwrap: F[Fix2[F, A]])

  enum Console[+A]:
    case Print(value: String, next: A)
    case Read(next: String => A)
    case Stop

  type ConsoleF[A] = Fix2[Console, A]

  val ask: ConsoleF[String] =
    Fix2.Program(Console.Print("What is your name?", Fix2.Program(Console.Read(Fix2.Value.apply))))

  val greet: String => ConsoleF[Unit] = name =>
    Fix2.Program(Console.Print(s"Hello, $name!", Fix2.Program(Console.Stop)))

// We want to compose ask with greet - an F[String] with a String => F[Unit], and get an F[Unit]. That's pretty
// clearly flatMap.

// We'll need F to be a functor though, if you look at the Fix2.Program part of map and flatMap
  given [F[_]: Functor]: Monad[Fix2[F, _]] with
    extension [A](a: A) def pure = Fix2.Value(a)

    extension [A](ffa: Fix2[F, A])
      def map[B](f: A => B) = ffa match
        case Fix2.Value(a)       => Fix2.Value(f(a))
        case Fix2.Program(value) => Fix2.Program(value.map(_.map(f)))

      def flatMap[B](f: A => Fix2[F, B]) = ffa match
        case Fix2.Value(a)       => f(a)
        case Fix2.Program(value) => Fix2.Program(value.map(_.flatMap(f)))

  given Functor[Console] with
    extension [A](fa: Console[A])
      def map[B](f: A => B) = fa match
        case Console.Print(value, next) => Console.Print(value, f(next))
        case Console.Read(next)         => Console.Read(next andThen f)
        case Console.Stop               => Console.Stop

  val prog = for
    name <- ask
    _    <- greet(name)
  yield ()

object v7:
  // Fix2 allows us to build a free monad from any functor F - let's call a cat a cat, that's Free
  enum Free[F[_], A]:
    case Pure(a: A)
    case Impure(unwrap: F[Free[F, A]])

  enum Console[+A]:
    case Print(value: String, next: A)
    case Read(next: String => A)
    case Stop

  given [F[_]: Functor]: Monad[Free[F, _]] with
    extension [A](a: A) def pure = Free.Pure(a)

    extension [A](ffa: Free[F, A])
      def map[B](f: A => B) = ffa match
        case Free.Pure(a)       => Free.Pure(f(a))
        case Free.Impure(value) => Free.Impure(value.map(_.map(f)))

      def flatMap[B](f: A => Free[F, B]) = ffa match
        case Free.Pure(a)       => f(a)
        case Free.Impure(value) => Free.Impure(value.map(_.flatMap(f)))

  given Functor[Console] with
    extension [A](fa: Console[A])
      def map[B](f: A => B) = fa match
        case Console.Print(value, next) => Console.Print(value, f(next))
        case Console.Read(next)         => Console.Read(next andThen f)
        case Console.Stop               => Console.Stop

  type ConsoleF[A] = Free[Console, A]

  def print(value: String): ConsoleF[Unit] = Free.Impure(Console.Print(value, Free.Pure(())))
  def read: ConsoleF[String]               = Free.Impure(Console.Read(str => Free.Pure(str)))
  def stop: ConsoleF[Unit]                 = Free.Impure(Console.Stop)

  // Those 3 functions follow exactly the same pattern:
  def lift[F[_]: Functor, A](fa: F[A]): Free[F, A] = Free.Impure(fa.map(Free.Pure.apply))

  def prog: ConsoleF[Unit] = for
    _    <- print("What is your name?")
    name <- read
    _    <- print(s"Hello, $name")
    _    <- stop
  yield ()

  // Finally, an interpreter:
  def eval(prog: ConsoleF[Unit]): Unit = prog match
    case Free.Pure(a) => a
    case Free.Impure(Console.Print(value, next)) =>
      println(value)
      eval(next)

    case Free.Impure(Console.Read(next)) =>
      eval(next(scala.io.StdIn.readLine))

    case Free.Impure(Console.Stop) => ()

// TODO: split this into its component: handling of Free, and handling of Console.
// TODO: realise that it's really quite painful to make Console an ADT - there might be a way to get a functor
// for any type constructor. That's CoYoneda
// TODO: once we have CoYoneda, can we refactor Free to hard-code CoYoneda? That is (or should, hopefully, be) Freer.
