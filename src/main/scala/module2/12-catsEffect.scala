package module2

import cats.effect.IO.Async
import cats.effect.concurrent.Ref
import cats.{Applicative, ApplicativeError, Apply, Defer, Eval, FlatMap, Functor, Monad, MonadError}
import cats.effect.{Async, Concurrent, ConcurrentEffect, ExitCase, ExitCode, IO, IOApp, Sync, SyncIO, Timer}

import scala.concurrent.duration.DurationInt
import cats.effect.implicits._
import cats.implicits.catsSyntaxApply

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, postfixOps}
import scala.util.Try
import cats.implicits._


import scala.io.StdIn

object typeClasses {


  def inc[F[_]: Applicative](el: F[Int]): F[Int] =
    Applicative[F].map(el)(i => i + 1)

  def tupled[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    Monad[F].flatMap(fa)(a => Monad[F].map(fb)(b => (a, b)))

  def mapN[F[_]: Monad, A, B, C](t: (F[A], F[B]))(f: (A, B) => C): F[C] =
    tupled[F, A, B](t._1, t._2).map{ case (a, b) =>
      f(a, b)
    }


  def echo[F[_]: Sync] = for{
    str <- Sync[F].delay(StdIn.readLine())
    _ <- Sync[F].delay(println(str))
  } yield ()

}


object A{

  // trait Either[+E, +A] -> F[_, _]
  def main(args: Array[String]): Unit = {

      type Eth[T] = Either[String, T]
//
//      typeClasses.inc[Option](Some(2))
//      typeClasses.inc[List](List(2))
//      typeClasses.inc[Eth](Right(2))
//
//      typeClasses.tupled[Option, Int, Int](Some(2), Some(3))
//      typeClasses.tupled[List, Int, Int](List(2), List(3))
//      typeClasses.tupled[Eth, Int, Int](Right(2), Right(3))

    val r1: toyCatsEffect.IO[Unit] = typeClasses.echo[toyCatsEffect.IO]
    r1.flatMap(_ => r1).run()
  }
}

object toyCatsEffect {

  case class IO[+A](run: () => A){

    def map[B](f: A => B): IO[B] = flatMap(a => IO.delay(f(a)))
    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(this.run()).run())
  }

  object IO {
    def delay[A](v: => A): IO[A] = IO(() => v)

    def fail[A](e: Throwable): IO[A] = IO[A](() => throw e)

    implicit val syncIO: Sync[toyCatsEffect.IO] = new Sync[toyCatsEffect.IO]{
      override def suspend[A](thunk: => IO[A]): IO[A] = IO(() => thunk.run())

      override def bracketCase[A, B](acquire: IO[A])(use: A => IO[B])(release: (A, ExitCase[Throwable]) => IO[Unit]): IO[B] = ???

      override def raiseError[A](e: Throwable): IO[A] = ???

      override def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] = ???

      override def pure[A](x: A): IO[A] = IO.delay(x)

      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???
    }
  }

}