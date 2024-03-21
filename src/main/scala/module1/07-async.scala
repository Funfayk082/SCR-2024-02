package module1

import module1.utils.NameableThreads

import java.io.File
import java.util.{Timer, TimerTask}
import java.util.concurrent.{Callable, Executor, ExecutorService, Executors, ForkJoinPool, ThreadFactory, ThreadPoolExecutor}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future, Promise, TimeoutException}
import scala.io.{BufferedSource, Source}
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

object threads {


  // Thread

  class Thread1 extends Thread{
    override def run(): Unit = {
      println(s"Hello from ${Thread.currentThread().getName}")
    }
  }


  def printRunningTime[A](v: => A): A = {
    val start = System.currentTimeMillis()
    val r = v
    val end = System.currentTimeMillis()
    println(s"Running time ${end - start}")
    r
  }

  // rates

  def getRatesLocation1 = {
    Thread.sleep(1000)
    println("GetRatesLocation1")
  }

  def getRatesLocation2 = {
    Thread.sleep(2000)
    println("GetRatesLocation2")
  }


  // async

  def async(f: => Unit): Thread = new Thread{
    override def run(): Unit = f
  }

  def getRatesLocation3 = async{
    Thread.sleep(1000)
    println("GetRatesLocation3")
  }

  def getRatesLocation4 = async{
    Thread.sleep(2000)
    println("GetRatesLocation4")
  }

  def async2[A](f: => A): A = {
    var v: A = null.asInstanceOf[A]
    val t = new Thread{
      override def run(): Unit = {
        v = f
      }
    }
    t.start()
    t.join()
    v
  }

  def getRatesLocation5 = async2{
    Thread.sleep(1000)
    println("GetRatesLocation3")
    10
  }

  def getRatesLocation6 = async2{
    Thread.sleep(2000)
    println("GetRatesLocation4")
    20
  }

  def getRatesLocation7: ToyFuture[Int] = ToyFuture{
    Thread.sleep(1000)
    println("GetRatesLocation3")
    10
  }(executor.pool1)

  def getRatesLocation8: ToyFuture[Int] = ToyFuture{
    Thread.sleep(2000)
    println("GetRatesLocation4")
    20
  }(executor.pool1)

  class ToyFuture[T] private (v: () => T){
    private var isCompleted: Boolean = false
    private var r: T = null.asInstanceOf[T]
    private val q = mutable.Queue[T => _]()

    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B] = ???
    def map[B](f: T => B): ToyFuture[B] = ???

    def onComplete[U](f: T => U): Unit = {
      if(isCompleted) f(r)
      else q.enqueue(f)
    }

    private def start(executor: Executor) = {
      val t = new Runnable{
        override def run(): Unit = {
          r = v()
          isCompleted = true
          while(q.nonEmpty){
            q.dequeue()(r)
          }
        }
      }
      executor.execute(t)
    }
  }

  object ToyFuture{
    def apply[T](v: => T)(executor: Executor): ToyFuture[T] = {
      val t1 = new ToyFuture[T](() => v)
      t1.start(executor)
      t1
    }
  }



}

object executor {
      val pool1: ExecutorService =
        Executors.newFixedThreadPool(2, NameableThreads("fixed-pool-1"))
      val pool2: ExecutorService =
        Executors.newCachedThreadPool(NameableThreads("cached-pool-2"))
      val pool3: ExecutorService =
        Executors.newWorkStealingPool(4)
      val pool4: ExecutorService =
        Executors.newSingleThreadExecutor(NameableThreads("singleThread-pool-4"))
}


object try_{

  def readFromFile(): List[Int] = {
    val s: BufferedSource = Source.fromFile(new File("ints.txt"))
    val result = try{
      s.getLines().toList.map(_.toInt)
    } catch {
      case e: Exception =>
        println(e.getMessage)
        Nil
    } finally {
      s.close()
    }
    result
  }

  def readFromFile2(): Try[List[Int]] = {
    val source: Try[BufferedSource] = Try(Source.fromFile(new File("ints.txt")))
    def lines(s: BufferedSource) =  Try(s.getLines().toList.map(_.toInt))
    val r = for{
      s <- source
      v <- lines(s)
    } yield v
    source.foreach(_.close())
    r
  }




}

object future{
  // constructors

  def longRunningComputation: Int = ???

   val f1: Future[Int] = Future(longRunningComputation)(scala.concurrent.ExecutionContext.global)
   val f2 = Future.successful(10)
   val f3 = Future.failed(new Throwable("Ooops"))
   val f4 = Future.fromTry(Try(longRunningComputation))


  def getRatesLocation1: Future[Int] = Future{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }(scala.concurrent.ExecutionContext.global)

  def getRatesLocation2: Future[Int] = Future{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }(scala.concurrent.ExecutionContext.global)



  // Execution contexts

  lazy val ec = ExecutionContext.fromExecutor(executor.pool1)
  lazy val ec2 = ExecutionContext.fromExecutor(executor.pool2)
  lazy val ec3 = ExecutionContext.fromExecutor(executor.pool3)
  lazy val ec4 = ExecutionContext.fromExecutor(executor.pool4)

  def printRunningTime[A](v: => Future[A]): Future[A] = {
    def log(start: Long, end: Long) = Future.fromTry(Try(println(s"Running time ${end - start}")))
//    Future.fromTry(Try(System.currentTimeMillis())).flatMap{s =>
//      v.flatMap{ r =>
//        Future.fromTry(Try(System.currentTimeMillis())).flatMap{ e =>
//          log(s, e).map{ _ =>
//            r
//          }(ec)
//        }(ec)
//      }(ec)
//    }(ec)

    implicit val iec = ec

    for{
      s <- Future.fromTry(Try(System.currentTimeMillis()))
      r <- v
      e <- Future.fromTry(Try(System.currentTimeMillis()))
      _ <-  log(s, e)
    } yield r
  }

  getRatesLocation1.onComplete {
    case Failure(exception) => ???
    case Success(value) => ???
  }(ec)

  val r: Unit = getRatesLocation1.foreach(i => println(i))(ec)
  getRatesLocation1.recover{
    case e: Throwable => 0
  }(ec)


  object FutureSyntax{
    def map[T, B](future: Future[T])(f: T => B)(implicit ec: ExecutionContext): Future[B] = {
      val p = Promise[B]
      future.onComplete {
        case Failure(exception) => p.failure(exception)
        case Success(value) => p.complete(Try(f(value)))
      }
      p.future
    }
  }

}