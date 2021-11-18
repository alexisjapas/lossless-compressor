import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class RLESpec extends AnyFlatSpec with Matchers
  {
    behavior of "compress.RLE"
     it must "exist" in
       { "val compressor : RLE[Char] = new RLE[Char]" must compile }
     it must "contains methods `compress` and `uncompress` with correct signature" in
       { """val compressor : RLE[Char] = new RLE[Char]
           |val z : Seq[(Char, Int)]  = compressor.compress("")
           |val s : Option[Seq[Char]] = compressor.uncompress(z)""".stripMargin must compile }

  }
