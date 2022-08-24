package gadtoop.exp.untyped;

class Literal implements Exp {
  private Integer value;
  
  public Literal(Integer value) {
    this.value = value;
  }

  public Object eval() {
    return value;
  }
}
