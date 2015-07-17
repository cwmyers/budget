package com.chrisandjo.finance.budget

import com.chrisandjo.finance.budget.model.{ReceivedTransaction, Transaction}

import scalaz._, Scalaz._

sealed trait AppAction[A] {
  def lift = Free.liftFC(this)
}

case class LoadKeyPairs(file: String, keyIndex: Int, valueIndex: Int) extends AppAction[AppError[List[(String, String)]]]

case class WriteToFile(file: String, output: String) extends AppAction[AppError[Unit]]
case class WriteTransactions(file: String, transactions: List[Transaction]) extends AppAction[AppError[Unit]]
case class LoadTransactions(file: String) extends AppAction[AppError[List[Transaction]]]
case class LoadMasterCardTransactions(file: String) extends AppAction[AppError[List[ReceivedTransaction]]]
case class LoadIngTransactions(file: String) extends AppAction[AppError[List[ReceivedTransaction]]]
case class LoadQtmbTransactions(file: String) extends AppAction[AppError[List[ReceivedTransaction]]]

case class ReportError(errors: String) extends AppAction[Unit]

object AppAction {
  type ScriptImpl[A] = Coyoneda[AppAction, A]
  type Script[A] = Free[ScriptImpl, A]

  implicit val monad = Free.freeMonad[Script]

  def loadKeyPairs(file: String, keyIndex: Int, valueIndex: Int): ScriptOrError[List[(String, String)]] = LoadKeyPairs(file, keyIndex, valueIndex).lift.lift
  def loadMasterCardTransactions(file: String): ScriptOrError[List[ReceivedTransaction]] = LoadMasterCardTransactions(file).lift.lift
  def loadIngTransactions(file: String): ScriptOrError[List[ReceivedTransaction]] = LoadIngTransactions(file).lift.lift
  def loadQTMBTransactions(file: String): ScriptOrError[List[ReceivedTransaction]] = LoadQtmbTransactions(file).lift.lift

  def writeToFile(file: String, output: String) = WriteToFile(file, output).lift.lift

  def writeTransactions(file:String, transactions:List[Transaction]) = WriteTransactions(file, transactions).lift.lift

  def loadResolvedTransactions(file:String) = LoadTransactions(file).lift.lift
  def printFailures(errors: List[String]) = errors map printFailure
  def printFailure(error: String) = ReportError(error).lift
  def noAction[A](a:A) = Monad[Script].pure(a)

}
