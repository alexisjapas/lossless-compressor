package lz

import compress.Compressor
import Dictionaries._


/** The LZW compression method
  * @param initialDictionary the starting dictionary
  */
class LZW(val initialDictionary : Dictionary = ASCII) extends Compressor[Char, Seq[Int]]
  {
    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[Int] = ???


    /** @inheritdoc */
    def uncompress(res : Seq[Int]) : Option[Seq[Char]] = ???
  }
