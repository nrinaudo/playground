package gadtoop.exp.untyped;

class Pair {
  private Object fst;
  private Object snd;

  public Pair(Object fst, Object snd) {
    this.fst = fst;
    this.snd = snd;
  }

  public Object getFirst() {
    return fst;
  }

  public Object getSecond() {
    return snd;
  }
}
