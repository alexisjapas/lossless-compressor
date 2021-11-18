package statistic

import compress.Compressor


/** A statistic compressor relies on the statistics of symbols in source
  * @param source the input source
  * @tparam S type of symbol used in source
  */
abstract class StatisticCompressor[S](source : Seq[S]) extends Compressor[S, Seq[Bit]]
  {
    /** A map giving the occurrences of each symbol in the source sequence */
    val occurrences : Map[S, Int] = source.groupBy(l => l).map(t => (t._1, t._2.length))

    /** SHANNON entropy of source */
    val entropy : Double = {
      val length : Int = source.size
      val ratios : Iterable[Double] = for (o <- occurrences) yield
        o._2.asInstanceOf[Double] / length * scala.math.log(o._2.asInstanceOf[Double] / length) / scala.math.log(2)

      -ratios.foldLeft(0.0) {
        (acc, num) => acc + num
      }
    }

    /** The sequence of occurrences sorted by count */
    val orderedCounts : Seq[(S, Int)] = occurrences.toSeq.sortBy(o => o._2)

    /** The encoding tree (in most cases, depends from `source`) */
    def tree : Option[EncodingTree[S]]

    /** @inheritdoc */
    def compress(msg: Seq[S]): Seq[Bit] = ???

    /** @inheritdoc */
    def uncompress(res: Seq[Bit]): Option[Seq[S]] = ???
  }
