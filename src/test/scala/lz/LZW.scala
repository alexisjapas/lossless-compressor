package lz

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import Dictionaries._

class LZWSpec extends AnyFlatSpec with Matchers
  {
    behavior of "compressor.lz.LZW"
     it must "exist" in
       { "val compressor : LZW = new LZW()" must compile }
     it must "contain methods `compress` and `uncompress` with correct signature" in
       { """val compressor : LZW  = new LZW()
           |val z : Seq[Int]  = compressor.compress(Seq.empty[Char])
           |val s : Option[Seq[Char]] = compressor.uncompress(z)""".stripMargin must compile }
     it must "contain field `initialDictionary` with correct type" in
       { """val compressor : LZW  = new LZW()
           |val dict : Dictionary = compressor.initialDictionary""".stripMargin must compile }
  }
