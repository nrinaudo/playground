package gadtoop.exp.typed;

class Pair<F, S> {
  private F fst;
  private S snd;

  public Pair(F fst, S snd) {
    this.fst = fst;
    this.snd = snd;
  }

  public F getFirst() {
    return fst;
  }

  public S getSecond() {
    return snd;
  }
}
