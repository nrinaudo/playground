package gadtoop.exp.untyped;

class And implements Exp {
  private Exp lhs;
  private Exp rhs;

  public And(Exp lhs, Exp rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  public Object eval() {
    return (Boolean)lhs.eval() && (Boolean)rhs.eval();
  }
}
