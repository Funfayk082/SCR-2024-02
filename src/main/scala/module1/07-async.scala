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

  def readFromFile() = ???


}

object future{
  // constructors

  def longRunningComputation: Int = ???





  // Execution contexts



}