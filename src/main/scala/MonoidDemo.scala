import cats.Monoid

object MonoidDemo extends App {
  import cats.instances.int._ // for Monoid
  import cats.syntax.semigroup._
  import cats.Monoid
  //import MonoidInstances.OrderMonoidInsatce

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = {
    items.foldLeft(monoid.empty)(_ |+| _)
  }


  println(add(List(1,2,3,4)))
  println(add(List(Option(1),Option(2),Option(3),Option(4))))
  val list = List(Order(1.1,2.0), Order(2.3,3.0))
  println(add(list))

}
case class Order(totalCost: Double, quantity: Double)

object Order{

  implicit val orderMonoidInsatce : Monoid[Order]= new Monoid[Order]{
    override def empty: Order = Order(0.0, 0.0)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

}


object MonoidInstances {

  import cats.Monoid

//  implicit def setMonoidInstance[A] = new Monoid[Set[A]]{
//    override def empty: Set[A] = Set.empty[A]
//
//    override def combine(x: Set[A], y: Set[A]): Set[A] = x.union(y)
//  }


}