package gadtoop.exp.eq;

class Tuple<F, S> extends Exp<Pair<F, S>> {
  private Exp<F> fst;
  private Exp<S> snd;

  public Tuple(Exp<F> fst, Exp<S> snd) {
    this.fst = fst;
    this.snd = snd;
  }

  public Pair<F, S> eval() {
    F f = fst.eval();
    S s = snd.eval();
    return new Pair<F, S>(f, s);
  }

  public boolean eq(Exp<Pair<F, S>> other) {
    return other.tupleEq(this);
  }

  public <A, B> boolean tupleEq(Tuple<A, B> other) {
    Tuple<F, S> cast = (Tuple<F, S>)other;
    return cast.fst == fst && cast.snd == snd;
  }
}
