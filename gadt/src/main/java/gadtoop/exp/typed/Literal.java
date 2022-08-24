package gadtoop.exp.typed;

class Literal implements Exp<Integer> {
  private Integer value;
  
  public Literal(Integer value) {
    this.value = value;
  }

  public Integer eval() {
    return value;
  }
}
