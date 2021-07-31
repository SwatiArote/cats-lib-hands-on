trait Equality[A]{
  def equal(a: A, b: A) : Boolean
}

object Equality{
  def apply[A] (implicit comparable : Equality[A]) = comparable
}


object ComparatorableInstances{

  implicit val intComparable = new Equality[Int] {
    override def equal(a: Int, b: Int): Boolean = a == b
  }

  implicit val catComparable = new Equality[Dog] {
    override def equal(a: Dog, b: Dog): Boolean = a.breed == b.breed
  }

}


object ComparatorableSyntax{

  implicit class ComparatorableOps[A](a:A)  {
    def +=(b:A)(implicit comparable: Equality[A]) = comparable.equal(a,b)
  }

}

case class Dog(name: String, age: Int, color: String,breed: String)

object EqualityDemo extends App {
 import  ComparatorableInstances._
  import ComparatorableSyntax._

  def equal[A: Equality](a: A, b:A) = Equality[A].equal(a,b)

  println(equal(2,3))
  println(equal(2,2))
  println(equal(Dog("mm",23,"white","Shepherd"),Dog("mm",23,"white","Shepherd")))
  println(equal(Dog("mm",23,"white","Shepherd"),Dog("mm",23,"white","Akita")))
  println(Dog("mm",23,"white","Shepherd")+=(Dog("mm",23,"white","Akita")))
  println(Dog("mm",23,"white","Shepherd")+=(Dog("mm",23,"white","Shepherd")))

  //Using cats lib
  println("******Cats lib**********")
  import cats.instances.option._
  import cats.Eq
  import cats.instances.int._
  import cats.syntax.eq._

  val eqInt = Eq[Int]

  println(eqInt.eqv(12, 11))
  println(eqInt.eqv(12, 12))
  println(12 === 12)
  println(11 =!= 11)
  println(Option(11) =!= Option(11))

  implicit val dogEq: Eq[Dog] =
    Eq.instance[Dog]{ (d1, d2) =>
      d1.breed == d2.breed
  }
  println(Dog("mm",23,"white","Shepherd") =!= (Dog("mm",23,"white","Shepherd")))
  val optionDog1 = Option(Dog("mm",23,"white","Shepherd"))
  val optionDog2 = Option.empty[Dog]
  println(optionDog1 === optionDog2)


}


// type class
//instances
//method
//syntax