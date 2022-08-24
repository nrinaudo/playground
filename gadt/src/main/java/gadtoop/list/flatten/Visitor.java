package gadtoop.list.flatten;

public interface Visitor<Element, Output> {
  Output visitCons(Element head, List<Element> tail);
  Output visitNil();
}