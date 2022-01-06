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
      def compress_bis(msg :Seq[Char], compressed:Seq[Int], dico:Dictionary, nb:Int) : Seq[Int]={
        if (msg.isEmpty) {
          compressed
        }
        else {
          val read = msg.take(nb).mkString("")

          //if only one char => this char is obviously in the dictionary => just put its ASCII translation in the compressed seq
          if (msg.length==1){
            compressed :+ dico.indexOf(msg.head.toString)
          }
          else{
            if (!dico.contains(read)){

              val newDico = dico :+ (read)
              compress_bis(msg.drop(nb-1),
                compressed:+newDico.indexOf(read.take(read.length-1)),
                newDico,
                2)
            }
            else {
              compress_bis(msg,
                compressed,
                dico,
                nb+1)
            }
          }
        }
      }
      if (msg.isEmpty){
        println("Please enter a non-empty sequence to compress")
        Seq()
      }
      else{
        compress_bis(msg, Seq(), initialDictionary, 2)
      }
    }


    /** @inheritdoc */
    def uncompress(res : Seq[Int]) : Option[Seq[Char]] = {
      @tailrec
      def uncompress_bis(res:Seq[Int], dico: Dictionary, uncompressed:Seq[Char], buffer:String): Option[Seq[Char]] ={
        if (res.isEmpty){
          Option(uncompressed)
        }
        else {
          val newBuffer = (buffer :+ dico(res.head)).mkString("")

          if (!dico.contains(newBuffer)){
            val newDico = dico :+ newBuffer
            uncompress_bis(res,
              newDico,
              uncompressed,
              "")

          }
          else{
            //uncompress Int only when it's defined in the dictionary
            uncompress_bis(res.drop(1),
              dico,
              uncompressed.concat(newBuffer.toList:Seq[Char]),
              newBuffer)
          }
        }
      }
      if (res.isEmpty){
        println("Please enter a non-empty sequence to uncompress")
        Option(null)
      }
      else{
        uncompress_bis(res , initialDictionary, Seq(), "")
      }
    }
  }
