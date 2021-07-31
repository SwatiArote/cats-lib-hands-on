trait Printable[A] {
     self =>
      def format(a: A) : String
      def contramap[B](func: B => A): Printable[B] = new Printable[B] {
        def format(value: B): String = self.format(func(value))
      }
}

object PrintableInstances{

  implicit val  stringPrintable = new Printable[String]{
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }

  implicit val  intPrintable = new Printable[Int]{
    override def format(a: Int): String = s"This is ${a.toString}"
  }

  implicit val  CatPrintable = new Printable[Cat]{
    override def format(a: Cat): String = s"${a.name} is a ${a.age} year-old ${a.color} cat."
  }

  implicit def printableInstancesOpt[A](implicit printable: Printable[A]) =
    new Printable[Option[A]] {
    println("suoper")
    override def format(opt: Option[A]): String = opt.fold("")(a => printable.format(a))
  }

  implicit val printableInstanceOfBoxString: Printable[Box[String]] = stringPrintable.contramap(_.value)
  implicit val printableInstanceOfBoxBoolean: Printable[Box[Boolean]] = booleanPrintable.contramap(_.value)

  //Invariant

  trait Codec[A] {
    self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B):String = self.encode(enc(value)) //contramap
      override def decode(value: String):B = dec(self.decode(value)) //map
    }
  }

  object Codec{
    implicit val stringCodec: Codec[String] =
      new Codec[String] {
        def encode(value: String): String = value
        def decode(value: String): String = value
      }

    implicit val intCodec: Codec[Int] =
      stringCodec.imap(_.toInt, _.toString)

    implicit val doubleCodec: Codec[Double] =
      stringCodec.imap(x => x.toDouble, _.toString)

    implicit def boxCodec[A](implicit codecOfA: Codec[A]) : Codec[Box[A]] = codecOfA.imap(Box(_), _.value)

  }

}
final case class Cat(name: String, age: Int, color: String)
final case class Box[A](value: A)


object PrintableSyntax{
  implicit class PrintableOps[A](a: A){
    def format(implicit pritable : Printable[A]) : String ={
            pritable.format(a)
    }

    def print(implicit prinatbel: Printable[A]) =println(prinatbel.format(a))
  }

}



object Demo extends App{
  import PrintableInstances._
  import PrintableSyntax._
  import cats.Show
  import cats.syntax.show._
  import Codec._

  def format[A](a: A)(implicit pritable: Printable[A]) = pritable.format(a)

  def print[A](a: A)(implicit pritable: Printable[A]) = println(format(a))

  print(2)
  print("type class")
  print(Cat("mm",23,"white"))
   2.print
   Cat("mm",23,"white").print

  implicit val showCat = new Show[Cat] {
    override def show(a: Cat): String = s"${a.name} is a ${a.age} year-old ${a.color} cat."
  }

  showCat.show(Cat("mm",23,"white"))
  Cat("mm",23,"white").show



  format(Box("hello world"))
  // res5: String = "hello world"
  format(Box(true))

  println(boxCodec[Double].encode(Box(123.4)))
 // boxCodec.decode[Box[Double]]("123.4")
  // res6: String = yes

}

// toEqual