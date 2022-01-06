package statistic

import scala.annotation.tailrec

/** The SHANNON-FANO compression method */
class ShannonFano[S](source : Seq[S]) extends StatisticCompressor[S](source)
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

      def sum_labels(points: Seq[EncodingTree[S]]): Int = {
        points.map(_.label).sum
      }

      def split_seq(leaves: Seq[EncodingTree[S]]) : Option[(Seq[EncodingTree[S]], Seq[EncodingTree[S]])] = {
        @tailrec
        def split_seq_bis(leaves: Seq[EncodingTree[S]], mid: Double, index: Int): Int = {
          if (sum_labels(leaves) <= mid) {
            index
          } else {
            split_seq_bis(leaves.dropRight(1), mid, index - 1)
          }
        }

        // CALL
        if (leaves.size < 2) {
          None
        } else {
          val mid = sum_labels(leaves).asInstanceOf[Double] / 2
          val index = split_seq_bis(leaves.dropRight(1), mid, leaves.size - 1)
          val first_split = leaves.splitAt(index)
          val second_split = leaves.splitAt(index + 1)
          val res_1 = math.abs(sum_labels(first_split._1) - sum_labels(first_split._2))
          val res_2 = math.abs(sum_labels(second_split._1) - sum_labels(second_split._2))
          Some(if (res_1 <= res_2) first_split else second_split)
        }
      }

      def tree_growth_from_roots(leaves: Seq[EncodingTree[S]]): EncodingTree[S] = {
        if (leaves.size > 1) {
          val left_split = split_seq(leaves).get._1
          val right_split = split_seq(leaves).get._2
          new EncodingNode[S](sum_labels(leaves),
            tree_growth_from_roots(left_split),
            tree_growth_from_roots(right_split))
        } else {
          leaves.head
        }
      }

      // CALL
      val leaves = tree_genesis_through_leaves_of_truth_that_get_power_from_divine_statistical_sunlight(orderedCounts.reverse)
      if (leaves.isDefined) {
        Some(tree_growth_from_roots(leaves.get))
      } else {
        None
      }
    }
  }
