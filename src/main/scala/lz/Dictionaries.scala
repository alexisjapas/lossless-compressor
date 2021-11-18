package lz

/** This object contains definitions for some dictionaries. */
object Dictionaries
  {
    /** A dictionary is an indexed sequence of strings */
    type Dictionary = IndexedSeq[String]

    /** The empty dictionary */
    val empty : Dictionary = IndexedSeq("")

    /** The ASCII table as a dictionary */
    val ASCII : Dictionary = (0 to 255) map (_.toChar.toString)
  }
