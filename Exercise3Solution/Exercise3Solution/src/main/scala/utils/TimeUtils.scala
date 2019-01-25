package utils

object TimeUtils {
  /**
    * Measures and prints the execution of a piece of code in milliseconds.
    *
    * Usage: TimeUtils.time{/*code to be timed*/}
    */
  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }
}