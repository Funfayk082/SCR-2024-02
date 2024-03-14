import module1.coll.Experiment

object Main {

  def main(args: Array[String]): Unit = {
    for (i <- 0 to 100) {
      func
    }
  }

  def func = {
    val experiments = List.fill(10000)(new Experiment)

    val result = experiments.map(_.takeBalls).count(_ == true)
    println(result.toDouble / experiments.length)
  }
}

