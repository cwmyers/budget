package com.chrisandjo.finance

import com.chrisandjo.finance.budget.AppAction._
import com.chrisandjo.finance.budget.interpreter.IOInterpreter

import scalaz._, Scalaz._
import scalaz.effect.IO
import org.apache.commons.csv.CSVRecord

package object budget {
  type AppError[A] = NonEmptyList[Errors] \/ A
  type VAppError[A] = Validation[NonEmptyList[Errors], A]

  type ScriptOrError[A] = EitherT[Script, NonEmptyList[Errors], A]

  type ListAppError[A] = ListT[AppErrorT, A]

  type IOAppError[A] = ListT[IO, AppError[A]]

  type AppErrorT[A] = EitherT[IO, NonEmptyList[Errors], A]

  type CSVList = ListT[IO, CSVRecord]

  implicit class ScriptAppOps[A](s: Script[AppError[A]]) {
    def lift = EitherT[Script, NonEmptyList[Errors], A](s)
  }

  implicit class AppErrorOps[A](a: AppError[A]) {
    def lift: ScriptOrError[A] = Monad[Script].pure(a).lift
  }

  implicit val doubleMonoid = new Monoid[Double] {
    override def zero: Double = 0.0

    override def append(f1: Double, f2: => Double): Double = f1 + f2
  }

  def run(script: ScriptOrError[Unit]): Unit =
    Free.runFC(resolveErrors(script))(IOInterpreter.interpret).unsafePerformIO()

  def resolveErrors(script: ScriptOrError[Unit]): Script[Unit] = {
    script.fold(errors => printFailure(handleFailures(errors)), noAction).flatMap(identity)
  }

  def handleFailures(l: NonEmptyList[Errors]): String = {
    l.foldMap {
      case UnknownStore(store) => s"Unknown store: $store\n"
      case BadNumber(message) => s"Bad Number: $message\n"
      case BadKeyPair(m) => s"Bad Key Pair: $m\n"
      case ErrorWritingFile(e) => s"Error writing file: ${e.getMessage}\n"
      case FileNotFound(e) => s"File not found: ${e.getMessage}\n"
    }
  }

}
