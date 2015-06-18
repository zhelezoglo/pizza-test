package pizza


import scala.language.implicitConversions

/**
 * Represents an order
 * @param receivedAt time when this order was received
 * @param procTime time needed to process this oder
 */
case class Order(receivedAt: Long, procTime: Long) extends Ordered[Order] {
  override def compare(that: Order): Int = {
    val cmpProcTimes = that.procTime.compareTo(this.procTime)
    if (cmpProcTimes != 0) cmpProcTimes
    else that.receivedAt.compareTo(this.receivedAt)
  }
}


object Order {

  implicit def from(str : String): Order = str match {
    case Order(receivedAt, procTime) => Order(receivedAt, procTime)
  }

  // in fact we can map Iterator[String] in Cook
  // to Iterator[Order] instead of using this extractor
  def unapply(orderStr: String): Option[(Long, Long)] =
    orderStr.split(" ") match {
      case Array(t: String, l: String) => Some((t.toLong, l.toLong))
      case _ => None
    }
}