package compress

/** Trait for compressors
 * A compressor converts a sequence of symbols to a new representation
 *
 * @tparam S type of the input symbols used
 * @tparam C type of the result of compression
 */
trait Compressor[S, C] {
  /** The compression method
   *
   * @param msg the input sequence to compress
   * @return the result of the compression
   */
  def compress(msg: Seq[S]): C

  /** The deflation method
   *
   * @param res the compressed result
   * @return the input sequence used for compression
   *         or [[None]] if `res` is not the result of the [[compress]] method
   */
  def uncompress(res: C): Option[Seq[S]]
}
