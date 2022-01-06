package statistic

import scala.annotation.tailrec

/** The HUFFMAN compression method */
class Huffman[S](source : Seq[S]) extends StatisticCompressor[S](source)
  {
    /** @inheritdoc */
    lazy val tree : Option[EncodingTree[S]] = {
      // FUNCTIONS
      def tree_genesis_through_leaves_of_truth_that_get_power_from_divine_statistical_sunlight(reversed_occurrences: Seq[(S, Int)]): Option[Seq[EncodingLeaf[S]]] = {
        if (reversed_occurrences.isEmpty) {
          None
        } else {
          Some(for (o <- reversed_occurrences) yield EncodingLeaf[S](o._2, o._1))
        }
      }

      @tailrec
      def tree_growth_from_leaves(tree_points: Seq[EncodingTree[S]]): Option[EncodingTree[S]] = {
        val new_label = tree_points.head.label + tree_points(1).label
        val new_node = EncodingNode[S](new_label, tree_points.head, tree_points(1))
        if (tree_points.size > 2) {
          tree_growth_from_leaves({tree_points.drop(2) :+ new_node}.sortBy(_.label))
        } else {  // tree_points.size == 2
          Some(new_node)
        }
      }

      // CALL
      val leaves = tree_genesis_through_leaves_of_truth_that_get_power_from_divine_statistical_sunlight(orderedCounts)
      if (leaves.isDefined) {
        tree_growth_from_leaves(leaves.get)
      } else {
        None
      }
    }
  }