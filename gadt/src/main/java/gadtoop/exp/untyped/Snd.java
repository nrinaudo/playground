package gadtoop.exp.untyped;

class Snd implements Exp {
  private Exp pair;

  public Snd(Exp pair) {
    this.pair = pair;
  }

  public Object eval() {
    return ((Pair)pair).getSecond();
  }
}
