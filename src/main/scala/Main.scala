
import module1.{executor, hof, lazyOps, list, threads, type_system}

import java.util.concurrent.Executor


object Main {

  def main(args: Array[String]): Unit = {
    println(s"Hello, World! - ${Thread.currentThread().getName}")


    // println(l1.drop(2))
//    val l3: list.List[Int] = for{
//      e1 <- l1
//      e2 <- l2
//    } yield e1 + e2
//
//    val l4: list.List[Int] = l1.flatMap(e1 => l2.map(e2 => e1 + e2))
//
//    val op1: Option[Int] = Some(1)
//    val op2: Option[Int] = Some(2)
//
//    val op3: Option[Int] = for{
//      e1 <- op1
//      e2 <- op2
//    } yield e1 + e2
//
//    op1.flatMap(e1 => op2.map(e2 => e1 + e2))

//    val t1 = new threads.Thread1
//    val t2 = new Thread{
//      override def run(): Unit = {
//        Thread.sleep(1000)
//        println(s"Hello from - ${Thread.currentThread().getName}")
//      }
//    }
//    t2.start()
//    t2.join()
//    t1.start()

    def combined: threads.ToyFuture[Int] = for{
      v1 <- threads.getRatesLocation7
      v2 <- threads.getRatesLocation8
    } yield  v1 + v2

    combined.onComplete(println)

    threads.printRunningTime(combined)



  }
}

