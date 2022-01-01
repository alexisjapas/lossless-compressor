import lz.LZW


class Test_RLE[T] {
  def test(msg : Seq[T]): Unit = {
    println(s"RLE on $msg:")
    val rle_compressor = new RLE[T]
    val compressed_data = rle_compressor.compress(msg)
    println(s"Compressed data: $compressed_data")
    val uncompressed_data = rle_compressor.uncompress(compressed_data)
    println(s"Uncompressed data: $uncompressed_data")
    if (uncompressed_data.isEmpty) {
      println("Empty entry data")
    } else {
      println(s"${if (uncompressed_data.get.equals(msg)) "OK" else "ko"}")
    }
    println()
  }
}


class Test_LZ {
  def test_lz78(msg : Seq[Char]) : Unit = {
    println(s"LZ78 on $msg:")
    val compressed_data = lz.LZ78.compress(msg)
    println(s"Compressed data: $compressed_data")
    val uncompressed_data = lz.LZ78.uncompress(compressed_data)
    println(s"Uncompressed data: $uncompressed_data")
    if (uncompressed_data.isEmpty) {
      println("Empty entry data")
    } else {
      println(s"${if (uncompressed_data.get.equals(msg)) "OK" else "ko"}")
    }
    println()
  }

  def test_lzw(msg : Seq[Char]) : Unit = {
    println(s"LZW on $msg:")
    val lzw_compressor = new LZW()
    val compressed_data = lzw_compressor.compress(msg)
    println(s"Compressed data: $compressed_data")
    val uncompressed_data = lzw_compressor.uncompress(compressed_data)
    println(s"Uncompressed data: $uncompressed_data")
    if (uncompressed_data.isEmpty) {
      println("Empty entry data")
    } else {
      println(s"${if (uncompressed_data.get.equals(msg)) "OK" else "ko"}")
    }
    println()
  }
}


object Main {
  def main(args: Array[String]): Unit = {
    val char_seq:Seq[Char] = Seq('b', 'e', 'l', 'l', 'e', ' ', 'e', 'c', 'h', 'e', 'l', 'l', 'e', ' ', '!')
    val int_seq:Seq[Int] = IndexedSeq(1, 1, 1, 2, 4, 4, 5, 5, 3, 2, 3, 3)
    val empty_int_seq:Seq[Int] = IndexedSeq()
    val seq_seq:Seq[Seq[Int]] = IndexedSeq(IndexedSeq(1, 2, 3), IndexedSeq(1, 2, 3), IndexedSeq(2))
    val string_seq:Seq[String] = IndexedSeq("Scalapi", "Scalapi", "Scapali", "Poum", "Poum")

    // Test
//    println("#### CHAR ####")
//    val test_char = new Test_RLE[Char]
//    test_char.test(char_seq)
//
//    println("#### INT ####")
//    val test_int = new Test_RLE[Int]
//    test_int.test(int_seq)
//    test_int.test(empty_int_seq)
//
//    println("#### SEQ ####")
//    val test_seq_seq = new Test_RLE[Seq[Int]]
//    test_seq_seq.test(seq_seq)
//
//    println("#### STRING ####")
//    val test_string = new Test_RLE[String]
//    test_string.test(string_seq)

    // TEST DICT
    println("#### DICTIONARIES ####")
    val test_lz = new Test_LZ
    test_lz.test_lz78(char_seq)
    test_lz.test_lzw("TOBEORNOTTOBEORTOBEORNOT")

  }
}
