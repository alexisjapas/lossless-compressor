package lz

import compress.Compressor

/** The LZ78 compression method */
object LZ78 extends Compressor[Char, Seq[(Int, Char)]]
  {
    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[(Int, Char)] = ???

    /** @inheritdoc */
    def uncompress(res : Seq[(Int, Char)]) : Option[Seq[Char]] = ???
  }
