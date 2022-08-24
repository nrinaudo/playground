package gadtoop.list.flatten;

class Nil<A> implements List<A> {
  public <Output> Output visit(Visitor<A, Output> visitor) {
    return visitor.visitNil();
  }

  public List<A> append(List<A> other) {
    return other;
  }

  public String toString() {
    return "nil";
  }
}
