package gadtoop.list.flatten;

class Cons<A> implements List<A> {
  private A head;
  private List<A> tail;

  public Cons(A head, List<A> tail) {
    this.head = head;
    this.tail = tail;
  }

  public List<A> append(List<A> other) {
    return new Cons<A>(head, tail.append(other));
  }

  public <Output> Output visit(Visitor<A, Output> visitor) {
    return visitor.visitCons(head, tail);
  }

  public String toString() {
    return head.toString() + " :: " + tail.toString();
  }  
}
