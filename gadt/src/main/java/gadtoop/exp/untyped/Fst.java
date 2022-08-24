package gadtoop.exp.untyped;

class Fst implements Exp {
  private Exp pair;

  public Fst(Exp pair) {
    this.pair = pair;
  }

  public Object eval() {
    return ((Pair)pair).getFirst();
  }
}
