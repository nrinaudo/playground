package gadtoop.exp.untyped;

class Subst implements Exp {
  private Exp lhs;
  private Exp rhs;

  public Subst(Exp lhs, Exp rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  public Object eval() {
    return (Integer)lhs.eval() - (Integer)rhs.eval();
  }
}
