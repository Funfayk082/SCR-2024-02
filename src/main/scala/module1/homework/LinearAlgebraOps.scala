package module1.homework

object LinearAlgebraOps{
  def sum(v1: Array[Int], v2: Array[Int]): Array[Int] = {
    if (v1.length != v2.length) throw new Exception("Operation is not supported")
    var result = new Array[Int](2)
    result(0) = v1(0)+v2(0)
    result(1) = v1(1)+v2(1)
    result
  }

  def scale(a: Int, v: Array[Int]): Array[Int] = {
    v.update(0, v(0)*a)
    v.update(1, v(1)*a)
    v
  }

  def axpy(a: Int, v1: Array[Int], v2: Array[Int]): Array[Int] = {
    val v: Array[Int] = scale(a, v1)
    sum(v, v2)
  }
}