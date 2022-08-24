package gadtoop

enum Tree[+Key, +Value]:
  case Leaf
  case Node(lhs: Tree[Key, Value], key: Key, value: Value, rhs: Tree[Key, Value])

object Tree:
  def find[Value](tree: Tree[Int, Value], key: Int): Option[Value] =
    tree match
      case Tree.Leaf                             => None
      case Tree.Node(_, k, value, _) if key == k => Some(value)
      case Tree.Node(lhs, k, _, _) if key < k    => find(lhs, key)
      case Tree.Node(_, _, _, rhs)               => find(rhs, key)
