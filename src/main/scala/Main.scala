import lz.LZW
import statistic.{Bit, EncodingLeaf, EncodingNode, Huffman, One, Zero}

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


object Main {
  def main(args: Array[String]): Unit = {
    val char_seq:Seq[Char] = Seq('b', 'e', 'l', 'l', 'e', ' ', 'e', 'c', 'h', 'e', 'l', 'l', 'e', ' ', '!')
    val int_seq:Seq[Int] = IndexedSeq(1, 1, 1, 2, 4, 4, 5, 5, 3, 2, 3, 3)
    val empty_int_seq:Seq[Int] = IndexedSeq()
    val seq_seq:Seq[Seq[Int]] = IndexedSeq(IndexedSeq(1, 2, 3), IndexedSeq(1, 2, 3), IndexedSeq(2))
    val string_seq:Seq[String] = IndexedSeq("Scalapi", "Scalapi", "Scapali", "Poum", "Poum")
    val char_seq1:Seq[Char] = Seq('b', 'e', 'l', 'l', 'e', 's', ' ', 'b', 'e', 'l', 'l', 'e', 's', ' ' , 'c', 'o', 'm', 'm', 'e', ' ', 'l', 'e', ' ', 'j', 'o', 'u', 'r', ' ', '!')
    val char_seq2:Seq[Char] = Seq('C', '\'', 'e', 's', 't', ' ', 'a', 's', 's', 'e', 'z', ',', ' ', 'd' , 'i', 't', ' ', 'l', 'a', ' ', 'b', 'a', 'l', 'e', 'i', 'n', 'e')
    val char_seq3:Seq[Char] = Seq('S', 'o', 'n', 't', '-', 'e', 'l', 'l', 'e', 's', ' ', 's', 'è', 'c' , 'h', 'e', 's', ' ', '?')
    val char_seq4:Seq[Char] = Seq('C', 'e', ' ', 'p', 'a', 'l', 'e', ' ', 'p', 'a', 'l', 'o', 'i', 's' , ' ', 'e', 's', 't', ' ', 'e', 'm', 'p', 'a', 'l', 'e', ' ' , 's', 'u', 'r', ' ', 'u', 'n', ' ', 'p', 'a', 'l', 'e')

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
//    println("#### DICTIONARIES ####")
//    val test_lz = new Test_LZ
//    test_lz.test_lz78("on on on on on onono")
//    test_lz.test_lzw(char_seq)
//    test_lz.test_lzw(char_seq1)
//    test_lz.test_lzw(char_seq2)
//    test_lz.test_lzw(char_seq3)
//    test_lz.test_lzw(char_seq4)
//
//    val seq = "on on on on onono"
//    test_lz.test_lzw(seq)

//    val to_un = Seq(111, 110, 32, 256, 258, 257, 259, 262, 110, 256, 111)
//    val te = new LZW()
//    println(te.uncompress(to_un))
    // aaabbc
//    val seq_aaabbc = Seq(Zero, Zero, Zero, One, Zero, One, Zero, One, One)
//    val feuille_a = new EncodingLeaf[Char](3, 'a')
//    val feuille_b = new EncodingLeaf[Char](2, 'b')
//    val feuille_c = new EncodingLeaf[Char](1, 'c')
//    val branche_c_b = new EncodingNode[Char](3, feuille_b, feuille_c)
//    val arbre = new EncodingNode[Char](6, feuille_a, branche_c_b)
//
//    println(arbre.decode(seq_aaabbc))

    val huffman_char = new Test_Huffman[Char]
    huffman_char.test(char_seq)
    huffman_char.test(char_seq1)
    huffman_char.test(char_seq2)
    huffman_char.test(char_seq3)
    huffman_char.test(char_seq4)

    val huffman_int = new Test_Huffman[Int]
    huffman_int.test(int_seq)

    val huffman_empty = new Test_Huffman[Int]
    huffman_empty.test(empty_int_seq)

    val huffman_string = new Test_Huffman[String]
    huffman_string.test(string_seq)

    val huffman_seq = new Test_Huffman[Seq[Int]]
    huffman_seq.test(seq_seq)
  }
}
