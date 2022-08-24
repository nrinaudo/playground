package gadtoop.exp.eq;

class Add extends Exp<Integer> {
  private Exp<Integer> lhs;
  private Exp<Integer> rhs;

  public Add(Exp<Integer> lhs, Exp<Integer> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  public Integer eval() {
    return lhs.eval() + rhs.eval();
  }
}
