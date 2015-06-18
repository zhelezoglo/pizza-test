package pizza

import scala.language.implicitConversions
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.io.Source

/**
 * Represents functionality for working with orders
 */
trait Cook {

  /**
   * Returns a future of iterator of orders(in a text resource) and their number
   * @param resourcePath name of the file to read
   * @return future of pair (iterator, numberOfOrders)
   */
  def getOrders(resourcePath: String): Future[(Iterator[String], Int)] = Future {
    val iterator = Source.fromURL(getClass.getResource(resourcePath)).getLines()
    (iterator, iterator.next().toInt)
  }

  /**
   *
   * @param orders iterator of strings which represent orders
   * @param numberOfOrders number of orders to process
   * @return
   */
  def ordersAvgWaitTime(orders: Iterator[String], numberOfOrders: Int): Future[Long] = Future {

    /**
     * Information about last processed order and orders left
     * @param orderReport info about last processed order
     * @param ordersLeft orders left
     */
    case class ProcessingReport(orderReport: OrderReport, ordersLeft: Int)

    /**
     * Information about processing of particular order
     * @param curTime current time after this order has been processed
     * @param waitTimeTotal waiting time for all processed orders including this
     */
    case class OrderReport(curTime: Long, waitTimeTotal: Long)

    // stores orders in the right order
    val unprocessed = mutable.PriorityQueue[Order]()

    /**
     * Processes `Order` using previous `OrderReport`
     * @param prevReport previous order report
     * @param order order to process now
     * @return report about processing the 'order'
     */
    def processOrder(prevReport: OrderReport, order: Order): OrderReport = {
      val curTimeNow =
        if (prevReport.curTime < order.receivedAt) order.receivedAt
        else prevReport.curTime
      val waitTimeThis = curTimeNow - order.receivedAt + order.procTime
      val curTime = curTimeNow + order.procTime
      OrderReport(curTime, prevReport.waitTimeTotal + waitTimeThis)
    }


    /**
     * Takes `ProcessingReport` about previously processed orders and
     * a `String` representation of the order to process now.
     * Returns `ProcessingReport` which includes info about current order.
     * @param procReport info about processed orders
     * @param orderStr order to process
     * @return report including info about `orderStr`
     */
    def process(procReport: ProcessingReport, orderStr: String): ProcessingReport = (procReport, orderStr) match {
      case (ProcessingReport(orderReport, ordersLeft), order @ Order(receivedAt, procTime)) =>
        val currentTime = orderReport.curTime
        val nextOrdersLeft = ordersLeft - 1
        val isLastOrder = nextOrdersLeft == 0
        isLastOrder match {
          case true if receivedAt > currentTime =>
            val procReportBeforeLast = (orderReport /: LazyIterator(unprocessed))(processOrder)
            ProcessingReport(processOrder(procReportBeforeLast, Order(receivedAt, procTime)), nextOrdersLeft)
          case true =>
            unprocessed.enqueue(order)
            val nextOrderReport = (orderReport /: LazyIterator(unprocessed))(processOrder)
            ProcessingReport(nextOrderReport, nextOrdersLeft)
          case false if receivedAt > currentTime =>
            val nextOrderReport = (orderReport /: LazyIterator(unprocessed))(processOrder)
            unprocessed.enqueue(order)
            ProcessingReport(nextOrderReport, nextOrdersLeft)
          case false =>
            unprocessed.enqueue(order)
            ProcessingReport(orderReport, nextOrdersLeft)
        }
    }

    val lastOrderReport = {
      // Assume that first order comes at time 0
      val firstProcReport = ProcessingReport(OrderReport(0, 0), numberOfOrders)
      val lastProcReport = (firstProcReport /: orders)(process)
      lastProcReport.orderReport
    }

    // Divide total waiting time of all orders by number of orders
    lastOrderReport.waitTimeTotal / numberOfOrders
  }

  /**
   * `dequeueAll` is not lazy in `mutable.PriorityQueue`
   * so it might be better to wrap it in a kind of `lazy` iterator
   * @param queue queue to wrap
   * @tparam A type of the queue elements
   */
  private case class LazyIterator[A](queue: mutable.PriorityQueue[A]) extends Iterator[A] {

    override def hasNext: Boolean = queue.nonEmpty

    override def next(): A = queue.dequeue()
  }
}
