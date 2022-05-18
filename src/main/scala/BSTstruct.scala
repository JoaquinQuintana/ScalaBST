sealed trait Node
case object NilNode extends Node
case class TreeNode[T](key: Int, data: T, left: Node, right:Node) extends Node