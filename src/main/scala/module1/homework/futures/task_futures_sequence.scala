package module1.homework.futures

import module1.homework.futures.HomeworksUtils.TaskSyntax

import scala.:+
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    @tailrec
    def tailRec(futures: List[Future[A]], results: Future[(List[A], List[Throwable])]): Future[(List[A], List[Throwable])] = futures match {
      case Nil => results
      case head :: tail =>
        val promise = Promise[(List[A], List[Throwable])]
        results.onComplete {
          case Success((successes, failures)) =>
            head.onComplete {
              case Success(value) => promise.success((successes :+ value, failures))
              case Failure(exception) => promise.success((successes, failures :+ exception))
            }
          case Failure(exception) => promise.failure(exception)
        }
        tailRec(tail, promise.future)
    }

    tailRec(futures, Future.successful((List.empty[A], List.empty[Throwable])))
  }
}
