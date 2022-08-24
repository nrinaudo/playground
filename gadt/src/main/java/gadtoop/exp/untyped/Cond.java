package gadtoop.exp.untyped;

class Cond implements Exp {
  private Exp cond;
  private Exp onTrue;
  private Exp onFalse;

  public Cond(Exp cond, Exp onTrue, Exp onFalse) {
    this.cond = cond;
    this.onTrue = onTrue;
    this.onFalse = onFalse;
  }

  public Object eval() {
    if((boolean)cond.eval())
      return onTrue.eval();
    return onFalse.eval();
  }
}
