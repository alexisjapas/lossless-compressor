package lz

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class LZ78Spec extends AnyFlatSpec with Matchers
  {
    behavior of "compressor.lz.LZ78"
     it must "exist" in
       { "val compressor = LZ78" must compile }
     it must "contain methods `compress` and `uncompress` with correct signature" in
       { """val compressor = LZ78
           |val z : Seq[(Int, Char)]  = compressor.compress(Seq.empty[Char])
           |val s : Option[Seq[Char]] = compressor.uncompress(z)""".stripMargin must compile }
  }
