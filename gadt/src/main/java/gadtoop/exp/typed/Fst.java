package gadtoop.exp.typed;

class Fst<T> implements Exp<T> {
  private Exp<Pair<T, ?>> pair;

  public Fst(Exp<Pair<T, ?>> pair) {
    this.pair = pair;
  }

  public T eval() {
    return pair.eval().getFirst();
  }
}
