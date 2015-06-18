package pizza

import org.scalatest._
import org.scalatest.concurrent.AsyncAssertions

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

import scala.language.postfixOps
import scala.concurrent.duration._


class TestCooking extends FlatSpec with Matchers with ParallelTestExecution with AsyncAssertions with Cook {

  def timed[A](block: => A) = {
    val t0 = System.nanoTime()
    val result = block
    info(s"Processing took ${(System.nanoTime() - t0) / 1000}*10E3 ms")
    result
  }

  val testInputsFolder = s"/pizza"

  val inputSources = Seq(
    ("orders0.txt", 9L),
    ("orders1.txt", 8L),
    ("orders2.txt", 7L),
    ("orders3.txt", 8L),
    ("orders4.txt", 16638357412815L)
  ).map {
    case (fileName, result) => (s"$testInputsFolder/$fileName", result)
  }

  it should "work for all inputs" in {
    inputSources.foreach {
      case (fileName: String, avg: Long) =>
        val waiter = new Waiter
        val averageWaitingTime = timed {
          for {
            (orders, numberOfOrders) <- getOrders(fileName)
            res <- ordersAvgWaitTime(orders, numberOfOrders)
          } yield res
        }
        averageWaitingTime.onComplete {
          case Success(result) =>
            waiter(result should be(avg))
            waiter.dismiss()
          case Failure(err) =>
            waiter(throw err)
            waiter.dismiss()
        }
        waiter.await(timeout(10 seconds))
    }
  }

}
