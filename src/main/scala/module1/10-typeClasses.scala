package module1

import module1.type_classes.JsValue.{JsNull, JsNumber, JsString}


object type_classes {

  sealed trait JsValue
  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue
    final case class JsString(get: String) extends JsValue
    final case class JsNumber(get: Double) extends JsValue
    final case object JsNull extends JsValue
  }

  // 1
  trait JsonWriter[T]{
    def write(v: T): JsValue
  }

  object JsonWriter{

    def apply[T](implicit ev: JsonWriter[T]): JsonWriter[T] = ev

    def from[T](f: T => JsValue): JsonWriter[T] = new JsonWriter[T] {
      override def write(v: T): JsValue = f(v)
    }

    implicit val intJsWriter = from[Int](JsNumber(_))

    implicit val strJsWriter = from[String](JsString)

    implicit def optJsWriter[A](implicit jsonWriter: JsonWriter[A]): JsonWriter[Option[A]] = from[Option[A]] {
      case Some(x) => jsonWriter.write(x)
      case None => JsNull
    }
  }

  implicit class JsonSyntax[T](v: T){
    def toJson(implicit jsonWriter: JsonWriter[T]) = jsonWriter.write(v)
  }


  // 3
  def toJson[T: JsonWriter](v: T): JsValue = {
    JsonWriter[T].write(v)
  }

  toJson("cddvdfv")
  toJson(10)
  toJson(Option(10))
  toJson(Option("fbffbf"))

  "dbvhbvf".toJson
  10.toJson
  Option("dvvf").toJson


  // 1
  trait Ordering[T]{
    def less(a: T, b: T): Boolean
  }

  object Ordering{

    def from[A](f: (A, A) => Boolean): Ordering[A] = new Ordering[A] {
      override def less(a: A, b: A): Boolean = f(a, b)
    }
    // 2
    implicit val intOrdering = from[Int](_ < _)

    implicit val strOrdering = from[String](_ < _)
  }


  // 3
  def _max[A](a: A, b: A)(implicit ordering: Ordering[A]): A =
    if(ordering.less(a, b)) b
    else a

  _max(10, 5)
  _max("ab", "bcd")

  // 1
  trait Eq[T]{
    def ===(a: T, b: T): Boolean
  }

  object Eq{

    def from[A](f: (A, A) => Boolean): Eq[A] = new Eq[A]{
      override def ===(a: A, b: A): Boolean = f(a, b)
    }

    implicit val strEq = from[String](_ == _)
  }

  // 4

  implicit class EqSyntax[A](a: A){
    // 3
    def ===(b: A)(implicit eq: Eq[A]): Boolean = eq.===(a, b)
  }

  val result = List("a", "b", "c").filter(str => str === "")
}
