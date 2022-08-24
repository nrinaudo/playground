package gadtoop.list.flatten;

interface List<A> {
  <Output> Output visit(Visitor<A, Output> visitor);

  List<A> append(List<A> other);

  static <U> List<U> flatten(List<List<U>> list) {
    return list.visit(new Visitor<List<U>, List<U>>() {

      public List<U> visitCons(List<U> head, List<List<U>> tail) {
        return head.append(flatten(tail));
      }

      public List<U> visitNil() {
        return new Nil<U>();
      }
    });
  }

  static <U> List<U> from(U... values) {
    List<U> acc = new Nil<U>();

    for(U value: values) {
      acc = new Cons<U>(value, acc);
    }

    return acc;
  }

  public static void main(String... args) {
    List<Integer> left = List.from(3, 2, 1);
    List<Integer> right = List.from(6, 5, 4);

    List<List<Integer>> composed = List.from(right, left);

    System.out.println(List.flatten(composed));
  }
 
}
