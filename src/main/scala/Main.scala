import module1.homework.LinearAlgebraOps


object Main {

  def main(args: Array[String]): Unit = {
    println("Hello, World!")
    var res1, res2 = Array[Int](1, 2)
    res1(0) = 2
    res1(1) = 5
    res2(0) = 14
    res2(1) = 1
    val a = 5
    val result = LinearAlgebraOps.axpy(a, res1, res2)
    println(result(0), result(1))
  }
}

