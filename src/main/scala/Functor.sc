//specific implementation for list
def map(list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case x::xs => (2 * x)+:  map(xs)
}

// generic implementation for list of any Type
def map1[A,B](list: List[A])(f: A => B): List[B] = list match {
  case Nil => Nil
  case x::xs => f(x)+:  map1(xs)(f)
}

// generic implementaion of any type (can be list or option)
object Functor{

  trait Functor[F[_]] {
    def map3[A, B](fa: F[A])(f: A => B): F[B]
  }

  val listFunctor = new Functor[List] {
    override def map3[A, B](fa: List[A])(f: A => B) = fa match {
      case Nil => Nil
      case x::xs => f(x)+:  map1(xs)(f)
    }
  }

  val optionFunctor = new Functor[Option] {
    override def map3[A, B](fa: Option[A])(f: A => B) = fa match {
      case None => None
      case Some(value)  => Some(f(value))
    }
  }


}


map(List(1,2,3))
map1(List(1,2,3))(x => x *2)
Functor.listFunctor.map3(List(1,2,3))(x => x *2)
Functor.optionFunctor.map3(Option(2))(x => x *2)
Functor.optionFunctor.map3(Some(5))(x => x *2)
Functor.optionFunctor.map3[Int, Int](None)(x => x *2)

import cats.Functor
sealed trait Tree[A]
final case class Branch[A](right: Tree[A], left: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

val treeFunctor = new Functor[Tree] {
  override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
    case Leaf(value) => Leaf(f(value))
    case Branch(right, left) => Branch(map(right)(f), map(left)(f))
  }
}

treeFunctor.map(Branch(Branch(Leaf(1),Leaf(2)), Leaf(4)))(x => x * 2)

val listFunctor = new Functor[List]{
  override def map[A, B](fa: List[A])(f: A => B):List[B] = fa match {
    case Nil => Nil
    case x::xs => f(x)+:map(xs)(f)

  }
}
