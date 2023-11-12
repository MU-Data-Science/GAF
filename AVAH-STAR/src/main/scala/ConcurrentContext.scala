/**
 * University of Missouri-Columbia
 * 2020
 */

// Futures/sliding window code is taken from http://www.russellspitzer.com/2017/02/27/Concurrency-In-Spark/
// Thanks to Russell Spitzer for excellent examples!
object ConcurrentContext {
  import scala.util._
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration.Duration
  import scala.concurrent.duration.Duration._
  /** Wraps a code block in a Future and returns the future */
  def executeAsync[T](f: => T): Future[T] = {
    Future(f)
  }

  /** Awaits only a set of elements at a time. At most batchSize futures will ever
   * be in memory at a time*/
  def awaitBatch[T](it: Iterator[Future[T]], batchSize: Int = 2, timeout: Duration = Inf) = {
    it.grouped(batchSize)
      .map(batch => Future.sequence(batch))
      .flatMap(futureBatch => Await.result(futureBatch, timeout))
  }

  def awaitSliding[T](it: Iterator[Future[T]], batchSize: Int = 2, timeout: Duration = Inf): Iterator[T] = {
    val slidingIterator = it.sliding(batchSize - 1) //Our look ahead (hasNext) will auto start the nth future in the batch
    val (initIterator, tailIterator) = slidingIterator.span(_ => slidingIterator.hasNext)
    initIterator.map( futureBatch => Await.result(futureBatch.head, timeout)) ++
      tailIterator.flatMap( lastBatch => Await.result(Future.sequence(lastBatch), timeout))
  }

  // Switch between sliding window vs batch
  def await[T](it: Iterator[Future[T]], batchSize: Int = 2, timeout: Duration = Inf) = {
    val ONE = 1
    if (batchSize.equals(ONE)) {
      awaitBatch(it, batchSize)
    }
    else {
      awaitSliding(it, batchSize)
    }
  }
}
