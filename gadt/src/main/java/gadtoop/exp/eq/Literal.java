package gadtoop.exp.eq;

class Literal extends Exp<Integer> {
  private Integer value;
  
  public Literal(Integer value) {
    this.value = value;
  }

  public Integer eval() {
    return value;
  }

  public boolean eq(Exp<Integer> other) {
    return other.litEq(this);
  }

  public boolean litEq(Literal other) {
    return value == other.value;
  }
}
