package gadtoop.exp.eq;

class Subst extends Exp<Integer> {
  private Exp<Integer> lhs;
  private Exp<Integer> rhs;

  public Subst(Exp<Integer> lhs, Exp<Integer> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  public Integer eval() {
    return (Integer)lhs.eval() - (Integer)rhs.eval();
  }
}
