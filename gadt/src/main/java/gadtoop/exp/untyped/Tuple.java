package gadtoop.exp.untyped;

class Tuple implements Exp {
  private Exp fst;
  private Exp snd;

  public Tuple(Exp fst, Exp snd) {
    this.fst = fst;
    this.snd = snd;
  }

  public Object eval() {
    return new Pair(fst.eval(), snd.eval());
  }
}
