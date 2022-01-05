package statistic

import scala.annotation.tailrec

/** The HUFFMAN compression method */
class Huffman[S](source : Seq[S]) extends StatisticCompressor[S](source)
  {
    /** @inheritdoc */
    lazy val tree : Option[EncodingTree[S]] = {
      def tree_genesis_through_leafs_of_truth_that_get_power_from_divine_statistical_sunlight(reversed_occurrences: Seq[(S, Int)]): Option[Seq[EncodingLeaf[S]]] = {
        if (reversed_occurrences.isEmpty) {
          None
        } else {
          Some(for (o <- reversed_occurrences) yield EncodingLeaf[S](o._2, o._1))
        }
      }

      @tailrec
      def tree_growth(tree_points: Seq[EncodingTree[S]]): Option[EncodingTree[S]] = {
        val new_label = tree_points.head.label + tree_points(1).label
        val new_node = EncodingNode[S](new_label, tree_points.head, tree_points(1))
        if (tree_points.size > 2) {
          tree_growth({tree_points.drop(2) :+ new_node}.sortBy(_.label))
        } else {  // tree_points.size == 2
          Some(new_node)
        }
      }

      val leafs = tree_genesis_through_leafs_of_truth_that_get_power_from_divine_statistical_sunlight(orderedCounts)
      if (leafs.isDefined) {
        tree_growth(leafs.head)
      } else {
        None
      }
    }
  }