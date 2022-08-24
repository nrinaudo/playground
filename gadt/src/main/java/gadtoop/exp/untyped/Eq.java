package gadtoop.exp.untyped;

class Eq implements Exp {
  private Exp lhs;
  private Exp rhs;

  public Eq(Exp lhs, Exp rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  public Object eval() {
    return ((Integer)lhs.eval()).equals((Integer)rhs.eval());
  }
  
}
