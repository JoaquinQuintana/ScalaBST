import scala.::

object run {
  case class KeyNotFound(key: Int) extends Exception

  def find[T](tree: Node, key: Int): T = {
    // YOUR CODE HERE
    tree match {
      //base case
      case TreeNode(k, d, _, _) if key == k => d.asInstanceOf[T]
      //search left
      case TreeNode(k, _, left,_) if key < k => find(left,key)
      //search right
      case TreeNode(k, _, _, right) if key > k => find(right,key)
      //we failed to find a key
      case _ =>  throw new KeyNotFound(key)

    }

    //all other case includes x = NilNode
    //else throw new KeyNotFound(key)
  }

  def insertKey[T](tree: Node, key: Int, data:T): Node = {

    tree match{
      //tree is currently empty
      case NilNode => new TreeNode(key, data, NilNode, NilNode)
      //if key is already present simply update data at key
      case TreeNode(k, d, left, right) if key == k =>TreeNode(key,data,left,right)
      //search left
      case TreeNode(k, d, left, right) if key < k =>
        val newleftTree = insertKey(left, key, data)
         TreeNode(k,d,newleftTree,right)
      //search right
      case TreeNode(k, d, left, right) if key > k =>
        val newrightTree = insertKey(right, key, data)
        TreeNode(k,d,left,newrightTree)
    }
  }

  def deleteKey[T](tree: Node, key: Int):Node  = tree match {

    case TreeNode(k, d, left, right) if key < k =>
      val newleftTree = deleteKey(left, key)
      TreeNode(k,d,newleftTree,right)

    case TreeNode(k, d, left, right) if key > k =>
      val newrightTree = deleteKey(right, key)
      TreeNode(k,d,left,newrightTree)

    case TreeNode(k, d, NilNode, NilNode) if key == k => {
      NilNode
    }
    // Left subtree is not nil but right node is Nil
    case TreeNode(k, d, left, NilNode) if key == k => {
      left
    }
    //Left subtree is nil but right is not nil
    case TreeNode(k, d,NilNode, right) if key == k => {
      right
    }
    //Both subtrees are not nil
    case TreeNode(k, d, left, right) if key == k => {
      //Both children are non-nil
      // 1. First find/delete "leftmost" node from the right subtree
      // 2. We have provided you  the helper function below
      //    This function gets you the left most key and data and the new right subtree with
      //      the leftmost subtree deleted

      def deleteLeftMostNodeInSubtree(tree: Node): (Int, T, Node) = {
        tree match {
          case TreeNode(k, d:T, NilNode, r) => (k, d, r)
          case TreeNode(k, d:T, left, r) => {
            val (k1, d1, left1) = deleteLeftMostNodeInSubtree(left)
            (k1, d1, TreeNode(k, d, left1, r))
          }
        }
      }

      val someTree = deleteLeftMostNodeInSubtree(tree)
      val newTree = deleteKey(someTree._3,key)
      TreeNode(someTree._1,someTree._2,left,newTree)
    }

  }

  def main (args: Array[String]): Unit = {
    //BEGIN TEST
    val t0 = NilNode
    val t1 = insertKey(t0, 10, "10")

    val t1_c :TreeNode[String] = t1.asInstanceOf[TreeNode[String]]
    assert(t1_c.key == 10)
    assert(t1_c.data == "10")

    val t2 = insertKey(t1, 5, "5")

    val t2_c: TreeNode[String] = t2.asInstanceOf[TreeNode[String]]
    assert(t2_c.key == 10)

    val t3 = insertKey(t2, 15, 15)
    val t3_c  = t3.asInstanceOf[TreeNode[Int]]
    val t3_cc = t3_c.right.asInstanceOf[TreeNode[Int]]
    assert (t3_cc.key == 15)
    assert (t3_cc.data == 15)

    val t4 = insertKey(t3, 25, "25")
    val t5 = insertKey(t4, 18, "18")
    val t6 = insertKey(t5, 10, 10)


    val t7 = deleteKey(t6, 25)
    assert (find(t7, 10) == 10)
    assert(find(t7, 18) == "18")
    assert(find(t7, 5) == "5")
    assert(find(t7, 15) == 15)
    try{
      find(t7, 25)
      assert(false, "key 25 did not get deleted")
    } catch {
      case KeyNotFound(k) => assert(k == 25)
    }

    val t8: Node = deleteKey(t6, 10)
    assert(find(t8, 18) == "18")
    assert(find(t8, 5) == "5")
    assert(find(t8, 15) == 15)
    assert(find(t8, 25) == "25")

    try{
      find(t8, 10)
      assert(false, "key 10 did not get deleted")
    } catch {
      case KeyNotFound(k) => assert(k == 10)
    }
}
}
