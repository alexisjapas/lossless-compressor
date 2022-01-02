package lz

import compress.Compressor
import Dictionaries._

import scala.annotation.tailrec

/** The LZW compression method
  * @param initialDictionary the starting dictionary
  */
class LZW(val initialDictionary : Dictionary = ASCII) extends Compressor[Char, Seq[Int]]
  {
    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[Int] = {
      @tailrec
      def smaller_not_in_dict(msg : Seq[Char], dict : lz.Dictionaries.Dictionary) : String = {
        if (dict.contains(msg.dropRight(1).mkString(""))) {
          msg.mkString("")
        } else {
          smaller_not_in_dict(msg.dropRight(1).mkString(""), dict)
        }
      }

      @tailrec
      def compress_bis(compressed : Seq[Int],
                       dict : lz.Dictionaries.Dictionary,
                       msg : Seq[Char]) : Seq[Int] = {
        if (dict.contains(msg.mkString(""))) {  // This condition exists because ASCII dictionary does not contain empty word
          compressed :+ dict.indexOf(msg.mkString(""))
        } else if (msg.isEmpty) {
          compressed
        } else {
          val add_to_dict : String = smaller_not_in_dict(msg, dict)
          compress_bis(compressed :+ dict.indexOf(add_to_dict.dropRight(1)),
            dict :+ add_to_dict,
            msg.drop(math.max(1, add_to_dict.length - 1)))
        }
      }

      // If not empty, start compressing
      if (msg.isEmpty) {
        println("Please enter non empty message to compress.")
        Seq()
      } else {
        compress_bis(Seq(), initialDictionary, msg)
      }
    }

    /** @inheritdoc */
    def uncompress(res : Seq[Int]) : Option[Seq[Char]] = {
      @tailrec
      def smaller_not_in_dict(msg : Seq[Char], dict : lz.Dictionaries.Dictionary) : String = {
        if (dict.contains(msg.dropRight(1).mkString(""))) {
          msg.mkString("")
        } else {
          smaller_not_in_dict(msg.dropRight(1).mkString(""), dict)
        }
      }

      @tailrec
      def uncompress_bis(res : Seq[Int],
                         uncompressed_word : Seq[Char],
                         dict : lz.Dictionaries.Dictionary,
                         buffer_to_dict : String): Seq[Char] = {
        if (res.isEmpty) {
          uncompressed_word
        }
        else if (buffer_to_dict.length < 2) {  // CARACTERE SEUL
          uncompress_bis(res.drop(1),
            (uncompressed_word :+ dict(res.head)).mkString(""),
            dict,
            (buffer_to_dict :+ dict(res.head)).mkString(""))
        }
        else {  // ENSEMBLE DE CARACTERES
          val to_dict = smaller_not_in_dict(buffer_to_dict, dict)
          if (dict.contains(to_dict)) {  // SI EXISTE DANS LE DICTIONNAIRE
            uncompress_bis(res.drop(1),
              (uncompressed_word :+ dict(res.head)).mkString(""),
              dict,
              (buffer_to_dict :+ dict(res.head)).mkString(""))
          }
          else {  // SI N'EXISTE PAS DANS LE DICTIONNAIRE
            uncompress_bis(res.drop(1),
              (uncompressed_word :+ (dict :+ to_dict)(res.head)).mkString(""),
              dict :+ to_dict,
              (buffer_to_dict :+ (dict :+ to_dict)(res.head)).drop(to_dict.length - 1).mkString(""))
          }
        }
      }

      // If not empty, start uncompressing
      if (res.isEmpty) {
        println("Please enter a non-empty sequence to uncompress")
        Option(null)
      } else {
        Option(uncompress_bis(res, "", initialDictionary, ""))
      }
    }
  }
