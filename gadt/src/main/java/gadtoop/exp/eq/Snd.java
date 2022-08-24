package gadtoop.exp.eq;

class Snd<T> extends Exp<T> {
  private Exp<Pair<?, T>> pair;

  public Snd(Exp<Pair<?, T>> pair) {
    this.pair = pair;
  }

  public T eval() {
    return pair.eval().getSecond();
  }
}
