package gadtoop.exp.eq;

abstract class Exp<T> {
  abstract T eval();

  boolean eq(Exp<T> other) {
    return false;
  }

  <A, B> boolean tupleEq(Tuple<A, B> other) {
    return false;
  }

  boolean litEq(Literal other) {
    return false;
  }
}
