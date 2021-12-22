import compress.Compressor

import scala.annotation.tailrec

/** The Run-length encoding compression method */
class RLE[T] extends Compressor[T, Seq[(T, Int)]]
  {
    /** @inheritdoc */
    def compress(msg : Seq[T]) : Seq[(T, Int)] = {
      @tailrec
      def compress_bis(msg : Seq[T], current_element : T, seq_tuples : Seq[(T, Int)], acc : Int) : Seq[(T, Int)] = {
        if (msg.isEmpty) {
          seq_tuples :+ (current_element, acc)
        } else {
          compress_bis(msg.drop(1),
            msg.head,
            if (current_element.equals(msg.head)) seq_tuples else seq_tuples :+ (current_element, acc),
            if (current_element.equals(msg.head)) acc + 1 else 1)
        }
      }
      if (msg.isEmpty) {
        println("Please enter non empty message to compress.")
        Seq()
      } else {
        compress_bis(msg.drop(1), msg.head, Seq(), 1)
      }
    }

    /** @inheritdoc */
    def uncompress(seq : Seq[(T, Int)]) : Option[Seq[T]] = {
      @tailrec
      def tupleUncompress(elm:T, n:Int, seq:Seq[T]): Seq[T] = {
        if (n==0) seq
        else {
          val new_seq = seq :+ elm
          tupleUncompress(elm, n-1, new_seq)
        }
      }
      @tailrec
      def uncompressBis(seq : Seq[(T, Int)], uncompressed_seq:Seq[T]): Seq[T] = {
        if (seq.isEmpty){
          uncompressed_seq
        } else{
          uncompressBis(seq.drop(1), tupleUncompress(seq.head._1, seq.head._2, uncompressed_seq))
        }
      }

      if (seq.isEmpty){
        println("Please enter a non-empty sequence to uncompress")
        Option(null)
      }
      else {
        Option(uncompressBis(seq, IndexedSeq()))
      }
    }
  }
