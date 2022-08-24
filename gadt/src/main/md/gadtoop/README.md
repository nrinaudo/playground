# Generalized Algebraic Data Types and Object-Oriented Programming

Paper: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/gadtoop.pdf

## Page 2
The following sentence is a little bit obscure:

> The root of the problem is that a virtual method must assume that the type of its receiver (this) is a generic
> instance of its class: it cannot impose nor make use of any pre-conditions on the class instantiation.
> Luckily, for functions over parameterized ADTs, such casts can always be avoided: we can re-code Flatten as a static
> method that uses a cast-free implementation of the Visitor pattern [6] to traverse its list, this time at a more
> specific instantiation.

The main problem, as far as I'm concerned, is the lack of sample implementation. This is provided in
`java/gadtoop/flatten`.

## Page 3
>  It’s not sensible to define equality between unevaluated typed Exp<T> (consider the case for Fst).

TODO: why?

## 2.2 GADTs

> The restriction that constructors all return ‘generic’ instances of the datatype can be removed.
Constructors of `Exp[T]` do not need to depend on `T` - eg, `Literal`, which is an `Exp[Int]`.

> The regularity restriction can be removed, permitting datatypes to be used at different types within their 
> own definition.

TODO: I do not understand this.

> A constructor can be allowed to mention additional type variables that may appear in its argument types
> but do not appear in its result type. The actual types at which such parameters are instantiated is not revealed
> by the type of the constructed term.

For example, `Fst[Left, Right](pair: Pair[Left, Right]) extends Exp[Left]`.
`Right` appears in `Fst`, but not in `Expt[Left]`.