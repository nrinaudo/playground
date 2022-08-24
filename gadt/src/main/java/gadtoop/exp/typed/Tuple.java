package gadtoop.exp.typed;

class Tuple<F, S> implements Exp<Pair<F, S>> {
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
}
