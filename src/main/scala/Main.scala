

object Main {
  /** Time difference since start in ms
    *
    * @param start Start time to calculate delta in ms
    * @return milliseconds passed since start
    */
  def timeSince(start: Long): Long = {
    ((System.nanoTime-start)/1e6).toLong
  }


  def main(args: Array[String]) {
    val start = System.nanoTime

    printf("Version %2d.%d %s\n", 0, 1, "You know, for starters.")

    println("time: "+ timeSince(start) +"ms")
  }
}