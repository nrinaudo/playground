package gadtoop.exp.typed;

class Cond<T> implements Exp<T> {
  private Exp<Boolean> cond;
  private Exp<T> onTrue;
  private Exp<T> onFalse;

  public Cond(Exp<Boolean> cond, Exp<T> onTrue, Exp<T> onFalse) {
    this.cond = cond;
    this.onTrue = onTrue;
    this.onFalse = onFalse;
  }

  public T eval() {
    if(cond.eval())
      return onTrue.eval();
    return onFalse.eval();
  }
}
