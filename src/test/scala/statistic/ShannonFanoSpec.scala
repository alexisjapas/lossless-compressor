package statistic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ShannonFanoSpec extends AnyFlatSpec with Matchers
  {
    behavior of "compressor.statistic.ShannonFano"
     it must "exist" in
       { "val compressor : ShannonFano[Char] = new ShannonFano(Seq.empty[Char])" must compile }
     it must "contain methods `compress` and `uncompress` with correct signature" in
       { """val compressor : ShannonFano[Char] = new ShannonFano(Seq.empty[Char])
           |val z : Seq[Bit]          = compressor.compress(Seq.empty[Char])
           |val s : Option[Seq[Char]] = compressor.uncompress(z)""".stripMargin must compile }
     it must "contain fields `entropy`, `occurrences` and `orderedCounts` with correct type" in
       { """val compressor : ShannonFano[Char] = new ShannonFano(Seq.empty[Char])
           |val h : Double = compressor.entropy
           |val occs : Map[Char, Int] = compressor.occurrences
           |val o_occs : Seq[(Char, Int)] = compressor.orderedCounts
           |val t : Option[EncodingTree[Char]] = compressor.tree""".stripMargin must compile }

  }
