// Type equality
sealed trait EQ[A, B]
final case class Refl[T]() extends EQ[T, T]

// Support for type constructor injectivity
//
// Note that this is limited in Scala. It works for concrete type constructors that
// are known statically to be injective, but the heuristic used is, according to
// XhtmlBoy, flawed (it would need "an ambivalent type system and a nominality tracker"):
// https://github.com/lampepfl/dotty/blob/b769da1e59cc0a6e1a1a540986c291dc8b7ae2e4/compiler/src/dotty/tools/dotc/core/TypeComparer.scala#L1104-L1124
//
// This reads:
// Given a proof of equality between List[X] and List[Y], I can prove that X and Y are the same type.
def inj[X, Y](eq: EQ[List[X], List[Y]]): EQ[X, Y] = eq match {
  case Refl() => Refl()
}

def sym[A, B](eq: EQ[A, B]): EQ[B, A] = eq match {
  case Refl() => Refl()
}

// Code that uses inj to provide a proof of local type equality.
def mk[X, Y](eq: EQ[List[X], List[Y]], x: X, y: Y): EQ[X, Y] = inj(eq)
def foo: EQ[Int, Int]                                        = mk(Refl(), 10, 20)
