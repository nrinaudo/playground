package printf

import scala.util.Try

// Now that Scala has proper GADTs with Scala 3, I was challenged by a group of friends to stretch my newly found GADT
// muscles and implement a fully typesafe printf and scanf.
//
// This is the result of that challenge. Not perfectly functional (our building blocks are rather poor), but I feel
// it serves as a proof of concept.
//
// We're treating formats as a sequence of segments, where each segment can be one of:
// - a string constant, known statically.
// - a variable, whose type is known statically but whose value will be given at runtime.
// - End Of Format, because recursive data types need an end-of-recursion marker.
//
// We're providing a "convenient" syntax for declaring formats:
// - raw strings map to string constants.
// - `int` maps to a numeric segment.
// - `string` maps to a string segment.
// - `end` maps to the end of the format.
//
// For example: `"Age: " ~: int ~: ", name: " ~: string ~: end
//
// We also try, in as much as possible, to have idiomatic signatures for printf and scanf.
//
// Of course, nothing is ever simple, and this raised a compiler bug: https://github.com/lampepfl/dotty/issues/15289

// - Data types --------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

// Describes the type of a dynamic "segment".
// If this code were to be for more than just demonstration purposes, these would take parameters (min and max length
// of a string, say).
enum Type[A]:
  case Str extends Type[String]
  case Num extends Type[Int]

// Describes a complete format.
// The type parameters are a little bit counter-intuitive:
// - `Handler` describes the type of the function that will be used to consume this format.
//   For example, `int ~: string ~: end` will have a handler type of `Int => String => Output`.
// - `Output` is the (context dependent) type that will returned when using this format. Printing will always be String,
//   while scanning will depend on what the caller does with the parsed segments.
enum Format[Handler, Output]:
  case Done[Output]() extends Format[Output, Output]
  case Const(const: String, next: Format[Handler, Output])
  case Var[Handler, Variable, Output](value: Type[Variable], next: Format[Handler, Output])
      extends Format[Variable => Handler, Output]

  // Syntax to make format definition a little bit nicer.
  def ~:[Variable](lhs: Type[Variable]): Format[Variable => Handler, Output] = Format.Var(lhs, this)
  def ~:(const: String): Format[Handler, Output]                             = Format.Const(const, this)

// Syntax helpers.
def end[A]: Format[A, A] = Format.Done[A]()
val int: Type[Int]       = Type.Num
val string: Type[String] = Type.Str

// - Printing ----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Printing is done by taking a `Format[Handler, String]` and returning a `Handler`.
//
// Remember that Handler is a *function* type, whose return value is the format's output - here, `String`.
// The handler of `int ~: int ~: end` will thus have type `Int => Int => String`: given two ints, returns the formatted
// string.
//
// Since Scala is not a curried-by-default language, `Int => Int => String` isn't a comfortable type to work with.
// For this reason, we provide uncurried versions, which unfortunately require quite a bit of boilerplate. This could
// easily automatically generated.

// Prints a given variable.
private def sprintVar[Variable](vType: Type[Variable], value: Variable): String = vType match
  case Type.Str => value
  case Type.Num => value.toString

// Builds a printer.
//
// This is a little bit tricky. One must remember that `Handler` is a function type. At each level of the recursion,
// we'll peel off one layer of `Handler` and `format` at the same time, which allows us to recurse on types that keep
// matching.
//
// The surprising `acc` function is used to accumulate the output string: until the end of `format`, we'll keep
// stacking layers of accumulator without every calling it. Once the recursion terminates with `Format.Done`,
// we can finally call the built up function with a concrete value, the empty string, and allow the entire chain
// of events to play itself backwards.
private def buildPrinter[Handler](format: Format[Handler, String], acc: String => String): Handler = format match
  case Format.Done()                    => acc("")
  case Format.Const(const, next)        => buildPrinter(next, str => acc(const + str))
  case Format.Var(vType: Type[t], next) => (value: t) => buildPrinter(next, str => acc(sprintVar(vType, value) + str))

// sprintf version that returns a curried handler.
//
// Scala developers will prefer the uncurried versions, much more idiomatic to the language.
private def sprintfCurried[Handler](format: Format[Handler, String]): Handler = buildPrinter(format, identity)

// Uncurried version of sprintf specialised to 2 parameters.
def sprintf[P1, P2, Function](format: Format[Function, String], p1: P1, p2: P2)(using
  Function =:= (P1 => P2 => String)
): String = sprintfCurried(format)(p1)(p2)

// Uncurried version of printf specialised to 2 parameters.
def printf[P1, P2, Function](format: Format[Function, String], p1: P1, p2: P2)(using
  Function =:= (P1 => P2 => String)
): Unit = println(sprintf(format, p1, p2))

// - Scanning ----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Scanning is done by taking a Format[Handler, Output] and a Handler, and calling the later with the parsed segments.
//
// For example, scanning `int ~: string ~: end` will parse parse an integer and a string, then pass them to a handler
// of type `Int => String => Output`.
// `Output` will be fixed by the specified handler.

// Attempts to read a variable of the specified type in the specified input.
// The return value encodes either an error message or a parsed value and the remainder of the input.
//
// This implementation is critically flawed: since I couldn't be bothered to write a proper parser for a proof of
// concept, it will treat any Type.Str as "read everything until the end of the input" - technically correct, even
// if only true when the next segment is `Format.Done`.
private def scanVar[Variable](varType: Type[Variable], input: String): Either[String, (Variable, String)] =
  varType match
    case Type.Str =>
      if input.isEmpty then Left("Expected string but found EOF")
      else Right((input, ""))

    case Type.Num =>
      val (num, remaining) = input.span(_.isDigit)

      if num.isEmpty then Left("Expected number but found EOF")
      else Try((num.toInt, remaining)).toEither.left.map(_ => s"Expected number but found $num")

// Remember that `Handler` is a function type - `Int => Int => Output`, say.
// This will attempt to read each segment of the format and pass it to the corresponding "layer" of hander
// (modulo errors).
//
// This turned out to be a lot more straightforward than sprintf, as there is no explicit accumulation needed.
// It's also very likely to be extremely stack unsafe.
private def scanfCurried[Handler, Output](format: Format[Handler, Output], input: String)(
  handler: Handler
): Either[String, Output] = format match
  // Fails on the non-empty string.
  case Format.Done() =>
    if input.isEmpty then Right(handler)
    else Left(s"Expected EOF but found $input")

  // Consumes `const` if possible, fails otherwise.
  case Format.Const(const, next) =>
    if input.startsWith(const) then scanfCurried(next, input.substring(const.length))(handler)
    else Left(s"Expected $const but found $input")

  // Consumes a variable of the specified type if possible, then recurses until `end` is encountered.
  case Format.Var(vType, next) =>
    for
      (value, remaining) <- scanVar(vType, input)
      output             <- scanfCurried(next, remaining)(handler(value))
    yield output

// Uncurried version of scanf, specialised to 2 parameters.
def scanf[P1, P2, Function, Output](format: Format[Function, Output], input: String)(handler: (P1, P2) => Output)(
  using(P1 => P2 => Output) =:= Function
): Either[String, Output] =
  scanfCurried(format, input)(handler.curried)

// - "Tests" -----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

@main def main =
  val formatted = sprintf("Height: " ~: int ~: ", weight: " ~: int ~: end, 210, 115)
  println(formatted)

  // Note the explicit type ascription in the handler - I unfortunately haven't (yet?) found a way to get type inference
  // to do its job here.
  val parsed = scanf("Height: " ~: int ~: ", weight: " ~: int ~: end, formatted)((h: Int, w: Int) => (h, w))
  println(parsed)

  assert(parsed == Right((210, 115)))
