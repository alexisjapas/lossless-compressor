package statistic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class EncodingTreeSpec extends AnyFlatSpec with Matchers
  {
    behavior of "compress.statistic.EncodingTree"
      it must "exist along with its child case classes" in
        { "val t : EncodingTree[Char] = EncodingNode(2, EncodingLeaf(1, 'a'), EncodingLeaf(1, 'b'))" must compile }
      it must "contain methods `has` and `meanLength` with correct signature" in
        { """val t : EncodingTree[Char] = EncodingNode(2, EncodingLeaf(1, 'a'), EncodingLeaf(1, 'b'))
            |val hasResult : Boolean = t has 'a'
            |val len       : Double  = t.meanLength """.stripMargin must compile }
      it must "contains extended methods `encode`, `decodeOnce` and `decode` with correct signature" in
        { """val et : EncodingTree[Char] = EncodingNode(2, EncodingLeaf(1, 'a'), EncodingLeaf(1, 'b'))
            |val z : Option[Seq[Bit]]          = et encode 'a'
            |val o : Option[(Char, Seq[Bit])]  = et decodeOnce Seq.empty[Bit]
            |val s : Option[Seq[Char]]         = et decode     Seq.empty[Bit]""".stripMargin must compile }
  }
