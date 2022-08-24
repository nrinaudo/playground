package gadtoop.exp.typed;

class And implements Exp<Boolean> {
  private Exp<Boolean> lhs;
  private Exp<Boolean> rhs;

  public And(Exp<Boolean> lhs, Exp<Boolean> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  public Boolean eval() {
    return lhs.eval() && rhs.eval();
  }
}
