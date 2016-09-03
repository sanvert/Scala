package scalashop

import org.scalameter._
import common._
import java.util.concurrent.ForkJoinTask

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
  // TODO implement this method using the `boxBlurKernel` method
    var x = 0
    var y = from
    while(y < end) {
      while(x < src.width) {
        dst.update(x, y, boxBlurKernel(src, x, y, radius))
        x += 1
      }
      x = 0
      y += 1
    }  
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
  // TODO implement using the `task` construct and the `blur` method
    def gatherAndJoin(ranges: List[(Int, Int)], taskList: List[ForkJoinTask[Unit]]): Unit = ranges match{
      case List() => taskList.foreach { x => x.join() }
      case x :: xs => gatherAndJoin(xs, task{blur(src, dst, x._1, x._2, radius)} :: taskList)
    }
    
    if(numTasks < 0) blur(src, dst, 0, src.height, radius)
    else {
      var taskList: List[ForkJoinTask[Unit]] = List[ForkJoinTask[Unit]]()
      val range = 0.to(src.height).by(Math.ceil(src.height.toDouble/numTasks).toInt)
      if(range.last != range.end) {
        task{blur(src, dst, range.last, src.height, radius)}
      }
      gatherAndJoin(range.zip(range.tail).toList, Nil)
    }    
  }

}
