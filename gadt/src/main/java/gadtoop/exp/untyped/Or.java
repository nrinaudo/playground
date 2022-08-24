package gadtoop.exp.untyped;

class Or implements Exp {
  private Exp lhs;
  private Exp rhs;

  public Or(Exp lhs, Exp rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  public Object eval() {
    return (Boolean)lhs.eval() || (Boolean)rhs.eval();
  }
}
