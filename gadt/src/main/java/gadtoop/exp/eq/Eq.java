package gadtoop.exp.eq;

class Eq extends Exp<Boolean> {
  private Exp<Integer> lhs;
  private Exp<Integer> rhs;

  public Eq(Exp<Integer> lhs, Exp<Integer> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  public Boolean eval() {
    return (lhs.eval()).equals(rhs.eval());
  }
  
}
