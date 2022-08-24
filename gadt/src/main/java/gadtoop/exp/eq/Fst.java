package gadtoop.exp.eq;

class Fst<T> extends Exp<T> {
  private Exp<Pair<T, ?>> pair;

  public Fst(Exp<Pair<T, ?>> pair) {
    this.pair = pair;
  }

  public T eval() {
    return pair.eval().getFirst();
  }
}
