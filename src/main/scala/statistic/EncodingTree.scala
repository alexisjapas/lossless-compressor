package statistic

import scala.annotation.tailrec

/** Trait for binary encoding trees (integer-labeled binary trees)
 *
 * @tparam S type of symbols (in leaves only)
  */
sealed abstract class EncodingTree[S](val label : Int)
  {
    /* OPERATIONS ON VALUES */

    /** Checks if tree contains given value
      * @param x value to search
      * @return true if the tree has a leaf with value `x`
      */
    def has(x : S) : Boolean = {
      this match {
        case tree: EncodingLeaf[S] =>  // If leaf, returns the existence of the value
          x == tree.value
        case tree: EncodingNode[S] =>  // If node, returns logical OR
          tree.right.has(x) | tree.left.has(x)
      }
    }

    /** Reduce operation on tree values when applying a function on leaves beforehand
      * @param f the function applied to each leaf value
      * @param op the aggregation operation on a node
      * @tparam U the result type of `f`
      * @return the aggregated value of the tree
      */
    def reduceWith[U](f : S => U)(op : (U, U) => U) : U = this match
      {
        case EncodingLeaf(_, v   ) => f(v)
        case EncodingNode(_, l, r) => op((l reduceWith f)(op), (r reduceWith f)(op))
      }

    /** Reduce operation on tree values
      *
      * `t reduce op` is a shorthand for `(t reduceWith {v => v})(op)`
      * @param op the aggregation operation on a node
      * @return the aggregated value of the tree
      */
    def reduce(op : (S, S) => S) : S = (this reduceWith {v => v})(op)


    /* OPERATIONS ON LABELS */

    /** Reduce operation on tree labels when applying a function on leaves beforehand
      * @param fL the function applied to each leaf label
      * @param opL the aggregation operation on a node : first parameter is the node label
      * @tparam A the result type of `f`
      * @return the result of aggregation operation recursively applied to tree
      */
    def reduceLabelWith[A](fL : Int => A)(opL : (Int, A, A) => A) : A = this match
      {
        case EncodingLeaf(lbl, _   ) => fL(lbl)
        case EncodingNode(lbl, l, r) => opL(lbl, (l reduceLabelWith fL)(opL), (r reduceLabelWith fL)(opL))
      }

    /** Reduce operation on tree labels
      *
      * `t reduceLabel opL` is a shorthand for `(t reduceLabelWith {lbl => lbl})(opL)`
      * @param opL the aggregation operation on a node : first parameter is the node label
      * @return the aggregated label of the tree
      */
    def reduceLabel(opL : (Int, Int, Int) => Int) : Int = (this reduceLabelWith identity)(opL)


    /* ENCODING/DECODING OPERATIONS */

    /** Computes the bit sequence corresponding to a tentative leaf value.
      * @param x value to encode
      * @return the corresponding bit sequence of `x` is a leaf of encoding tree, `None` otherwise
      */
    def encode(x : S) : Option[Seq[Bit]] = {
      def encode_bis(to_encode: S, bits: Seq[Bit], tree: EncodingTree[S]) : Option[Seq[Bit]] = {
        tree match {
          case this_tree: EncodingLeaf[S] =>  // If value sought in leaf, returns bits sequence
            if (this_tree.value == to_encode) Some(bits) else None
          case this_tree: EncodingNode[S] =>  // If node, returns logical OR
            val left_sub = encode_bis(to_encode, bits :+ Zero, this_tree.left)
            val right_sub = encode_bis(to_encode,bits :+ One, this_tree.right)
            if (left_sub.isDefined) {
              left_sub
            } else if (right_sub.isDefined) {
              right_sub
            } else {
              None
            }
        }
      }
      encode_bis(x, Seq(), this)
    }

    /** Computes the next value corresponding to the beginning of bit sequence (if possible)
      * @param res the bit sequence to decode
      * @return the decoded value and the bit sequence left to be decoded or `None` if current bit sequence does not lead to a leaf in encoding tree
      */
    def decodeOnce(res : Seq[Bit]) : Option[(S, Seq[Bit])] = {
      def tree_depth(tree: EncodingTree[S], counter: Int): Int = {
        tree match {
          case _: EncodingLeaf[S] =>
            counter
          case this_tree: EncodingNode[S] =>
            math.max(tree_depth(this_tree.left, counter + 1), tree_depth(this_tree.right, counter + 1))
        }
      }

      val depth = tree_depth(this, 0)

      @tailrec
      def is_there_a_way(bit_code: Seq[Bit], tree: EncodingTree[S]) : Option[S] = {
        if (bit_code.isEmpty) {
          tree match {
            case this_tree: EncodingLeaf[S] =>
              Some(this_tree.value)
            case _: EncodingNode[S] =>
              None
          }
        } else {
          tree match {
            case _: EncodingLeaf[S] =>
              None
            case this_tree: EncodingNode[S] =>
              is_there_a_way(bit_code.drop(1), if (bit_code.head == Zero) this_tree.left else this_tree.right)
          }
        }
      }

      @tailrec
      def decodeOnce_bis(res : Seq[Bit], bit_code: Seq[Bit]) : Option[(S, Seq[Bit])] = {
        val way = is_there_a_way(bit_code, this)
        if (way.isDefined) {
          Some(way.head, bit_code)
        } else if (res.isEmpty) {
          None
        } else if (bit_code.length > depth) {
          None
        } else {
          decodeOnce_bis(res.drop(1), bit_code :+ res.head)
        }
      }
      decodeOnce_bis(res.drop(1), Seq(res.head))
    }

    /** Computes the sequence of values from the sequence of bits
      * @param res the bit sequence to decode
      * @return the sequence of decoded values or `None` otherwise
      */
    def decode(res : Seq[Bit]) : Option[Seq[S]] = {
      @tailrec
      def decode_bis(res: Seq[Bit], decoded: Seq[S]): Option[Seq[S]] = {
        if (res.isEmpty) {
          Some(decoded)
        } else {
          val to_decoded = decodeOnce(res)
          if (to_decoded.isDefined) {
            decode_bis(res.drop(to_decoded.head._2.length), decoded :+ to_decoded.head._1)
          } else {
            None
          }
        }
      }
      decode_bis(res, Seq())
    }


    /* MISCELLANEOUS */

    /** Mean length of code associated to encoding tree */
    lazy val meanLength : Double = {
      def counter(count: Int, max: Int, tree: EncodingTree[S]) : Double = {
        tree match {
          case this_tree: EncodingLeaf[S] =>  // If leaf, returns the existence of the value
            count.asInstanceOf[Double] * this_tree.label.asInstanceOf[Double] / max
          case this_tree: EncodingNode[S] =>  // If node, returns logical OR
            counter(count+1, max, this_tree.left) + counter(count+1, max, this_tree.right)
        }
      }
      counter(0, this.label, this)
    }

    /** @inheritdoc */
    override def toString : String = this match
     {
       case EncodingLeaf(lbl, v   ) => (v, lbl).toString()
       case EncodingNode(lbl, l, r) => s"EncodingNode([$lbl], $l, $r)"
     }
  }
case class EncodingNode[S](override val label : Int, left : EncodingTree[S], right : EncodingTree[S]) extends EncodingTree[S](label)
case class EncodingLeaf[S](override val label : Int, value : S) extends EncodingTree[S](label)

