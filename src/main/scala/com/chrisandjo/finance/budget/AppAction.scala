package com.chrisandjo.finance.budget

import com.chrisandjo.finance.budget.model.Transaction

import scalaz._, Scalaz._

sealed trait AppAction[A]

case class LoadKeyPairs(file: String, keyIndex: Int, valueIndex: Int) extends AppAction[AppError[List[(String, String)]]]

case class WriteToFile(file: String, output: String) extends AppAction[AppError[Unit]]
case class WriteTransactions(file: String, transactions: List[Transaction]) extends AppAction[AppError[Unit]]
case class LoadTransactions(file: String) extends AppAction[AppError[List[Transaction]]]

case class ReportError(errors: String) extends AppAction[Unit]

object AppAction {
  type ScriptImpl[A] = Coyoneda[AppAction, A]
  type Script[A] = Free[ScriptImpl, A]

  implicit val monad = Free.freeMonad[Script]

  def loadKeyPairs(file: String, keyIndex: Int, valueIndex: Int): ScriptOrError[List[(String, String)]] = Free.liftFC(LoadKeyPairs(file, keyIndex, valueIndex)).lift

  def writeToFile(file: String, output: String) = Free.liftFC(WriteToFile(file, output)).lift

  def writeTransactions(file:String, transactions:List[Transaction]) = Free.liftFC(WriteTransactions(file, transactions)).lift

  def loadResolvedTransactions(file:String) = Free.liftFC(LoadTransactions(file)).lift
  def printFailures(errors: List[String]) = errors map printFailure
  def printFailure(error: String) = Free.liftFC(ReportError(error))
  def noAction[A](a:A) = Monad[Script].pure(a)

}
