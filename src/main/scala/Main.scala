import lz.LZW
import statistic.{Huffman, ShannonFano}

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

class Test_Huffman[S] {
  def test(msg : Seq[S]): Unit = {
    println(s"Huffman on $msg:")
    val huffman_compressor = new Huffman[S](msg)
    val compressed_data = huffman_compressor.compress(msg)
    println(s"Compressed data: $compressed_data")
    val uncompressed_data = huffman_compressor.uncompress(compressed_data)
    println(s"Uncompressed data: $uncompressed_data")
    if (uncompressed_data.isEmpty) {
      println("Empty entry data")
    } else {
      println(s"${if (uncompressed_data.get.equals(msg)) "OK" else "ko"}")
    }
    println()
  }
}

class Test_ShannonFano[S] {
  def test(msg : Seq[S]): Unit = {
    println(s"Shannon Fano on $msg:")
    val sf_compressor = new ShannonFano[S](msg)
    val compressed_data = sf_compressor.compress(msg)
    println(s"Compressed data: $compressed_data")
    val uncompressed_data = sf_compressor.uncompress(compressed_data)
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
    val empty_int_seq:Seq[Int] = IndexedSeq()
    val char_seq:Seq[Char] = Seq('b', 'e', 'l', 'l', 'e', ' ', 'e', 'c', 'h', 'e', 'l', 'l', 'e', ' ', '!')
    val char_seq1:Seq[Char] = Seq('b', 'e', 'l', 'l', 'e', 's', ' ', 'b', 'e', 'l', 'l', 'e', 's', ' ' , 'c', 'o', 'm', 'm', 'e', ' ', 'l', 'e', ' ', 'j', 'o', 'u', 'r', ' ', '!')
    val char_seq2:Seq[Char] = Seq('C', '\'', 'e', 's', 't', ' ', 'a', 's', 's', 'e', 'z', ',', ' ', 'd' , 'i', 't', ' ', 'l', 'a', ' ', 'b', 'a', 'l', 'e', 'i', 'n', 'e')
    val char_seq3:Seq[Char] = Seq('S', 'o', 'n', 't', '-', 'e', 'l', 'l', 'e', 's', ' ', 's', 'è', 'c' , 'h', 'e', 's', ' ', '?')
    val char_seq4:Seq[Char] = Seq('C', 'e', ' ', 'p', 'a', 'l', 'e', ' ', 'p', 'a', 'l', 'o', 'i', 's' , ' ', 'e', 's', 't', ' ', 'e', 'm', 'p', 'a', 'l', 'e', ' ' , 's', 'u', 'r', ' ', 'u', 'n', ' ', 'p', 'a', 'l', 'e')
    val string_seq:Seq[String] = IndexedSeq("Scalapi", "Scalapi", "Scapali", "Poum", "Poum")
    val int_seq:Seq[Int] = IndexedSeq(1, 1, 1, 2, 4, 4, 5, 5, 3, 2, 3, 3)
    val seq_seq:Seq[Seq[Int]] = IndexedSeq(IndexedSeq(1, 2, 3), IndexedSeq(1, 2, 3), IndexedSeq(2))

    // RLE
    println("########## RLE ##########")
    val test_char = new Test_RLE[Char]
    val test_int = new Test_RLE[Int]
    val test_string = new Test_RLE[String]
    val test_seq_seq = new Test_RLE[Seq[Int]]

    test_int.test(empty_int_seq)
    test_char.test(char_seq)
    test_char.test(char_seq1)
    test_char.test(char_seq2)
    test_char.test(char_seq3)
    test_char.test(char_seq4)
    test_string.test(string_seq)
    test_int.test(int_seq)
    test_seq_seq.test(seq_seq)


    // TEST DICT
    println("########## DICTIONARIES ##########")
    val test_lz = new Test_LZ
    test_lz.test_lz78(char_seq)
    test_lz.test_lz78(char_seq1)
    test_lz.test_lz78(char_seq2)
    test_lz.test_lz78(char_seq3)
    test_lz.test_lz78(char_seq4)

    test_lz.test_lzw(char_seq)
    test_lz.test_lzw(char_seq1)
    test_lz.test_lzw(char_seq2)
    test_lz.test_lzw(char_seq3)
    test_lz.test_lzw(char_seq4)

    // TESTS STATS
    println("########## STATISTICS ##########")
    val huffman_empty = new Test_Huffman[Int]
    huffman_empty.test(empty_int_seq)

    val huffman_char = new Test_Huffman[Char]
    huffman_char.test(char_seq)
    huffman_char.test(char_seq1)
    huffman_char.test(char_seq2)
    huffman_char.test(char_seq3)
    huffman_char.test(char_seq4)

    val huffman_string = new Test_Huffman[String]
    huffman_string.test(string_seq)

    val huffman_int = new Test_Huffman[Int]
    huffman_int.test(int_seq)

    val huffman_seq = new Test_Huffman[Seq[Int]]
    huffman_seq.test(seq_seq)

    val sf_empty = new Test_ShannonFano[Int]
    sf_empty.test(empty_int_seq)

    val sf_char = new Test_ShannonFano[Char]
    sf_char.test(char_seq)
    sf_char.test(char_seq1)
    sf_char.test(char_seq2)
    sf_char.test(char_seq3)
    sf_char.test(char_seq4)

    val sf_string = new Test_ShannonFano[String]
    sf_string.test(string_seq)

    val sf_int = new Test_ShannonFano[Int]
    sf_int.test(int_seq)

    val sf_seq = new Test_ShannonFano[Seq[Int]]
    sf_seq.test(seq_seq)
  }
}
