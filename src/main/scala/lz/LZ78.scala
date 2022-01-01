package lz

import compress.Compressor

import scala.annotation.tailrec

/** The LZ78 compression method */
object LZ78 extends Compressor[Char, Seq[(Int, Char)]]
  {
    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[(Int, Char)] = {
      @tailrec
      def word_to_dict(msg : Seq[Char], dict : lz.Dictionaries.Dictionary) : String = {
        if (dict.contains(msg.dropRight(1).mkString(""))) {
          msg.mkString("")
        } else {
          word_to_dict(msg.dropRight(1).mkString(""), dict)
        }
      }

      @tailrec
      def compress_bis(compressed : Seq[(Int, Char)],
                       dict : lz.Dictionaries.Dictionary,
                       msg : Seq[Char]) : Seq[(Int, Char)] = {
        if (msg.isEmpty) {
          compressed
        } else {
          val add_to_dict : String = word_to_dict(msg, dict)
          compress_bis(compressed :+ (dict.indexOf(add_to_dict.dropRight(1)), add_to_dict.last),
            dict :+ add_to_dict,
            msg.drop(add_to_dict.length))
        }
      }

      // If not empty, start compressing
      if (msg.isEmpty) {
        println("Please enter non empty message to compress.")
        Seq()
      } else {
        compress_bis(Seq(), lz.Dictionaries.empty, msg)
      }
    }

    /** @inheritdoc */
    def uncompress(res : Seq[(Int, Char)]) : Option[Seq[Char]] = {
      @tailrec
      def uncompress_bis(res : Seq[(Int, Char)],
                         uncompressed_word : Seq[Char],
                         dict : lz.Dictionaries.Dictionary): Seq[Char] = {
        if (res.isEmpty) {
          uncompressed_word
        } else {
          uncompress_bis(res.drop(1),
            (uncompressed_word :+ dict(res.head._1) + res.head._2).mkString(""),
            dict :+ dict(res.head._1) + res.head._2)
        }
      }

      // If not empty, start uncompressing
      if (res.isEmpty) {
        println("Please enter a non-empty sequence to uncompress")
        Option(null)
      } else {
        Option(uncompress_bis(res, "", lz.Dictionaries.empty))
      }
    }
  }
