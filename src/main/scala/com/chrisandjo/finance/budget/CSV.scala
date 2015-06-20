package com.chrisandjo.finance.budget

import com.chrisandjo.finance.budget.AppAction.{Script, loadKeyPairs}
import com.chrisandjo.finance.budget.Budget.Transaction
import org.apache.commons.csv.CSVRecord

import scalaz.Scalaz._
import scalaz._

object CSV {

  val scriptListFunctor = Functor[ScriptOrError] compose Functor[List]
  val scriptAppErrorFunctor = Functor[Script] compose Functor[AppError]
  val appErrorListFunctor = Functor[AppError] compose Functor[List]
  val scriptAppList = scriptAppErrorFunctor compose Functor[List]

  def negate[A](a: (A, Double)): (A, Double) =
    a.copy(_2 = -a._2)

  def negate(a: Transaction): Transaction =
    a.copy(amount = -a.amount)


  def csvRecordToTransaction(dateIndex: Int, descriptionIndex: Int, valueIndex: Int)(r: CSVRecord): AppError[Transaction] = {
    \/.fromTryCatchNonFatal {
      Transaction(r.get(dateIndex), r.get(descriptionIndex), r.get(valueIndex).toDouble)
    } leftMap (e => NonEmptyList(BadKeyPair(s"$e $r")))
  }


  //  def loadTransactions(fileName: String, dateIndex: Int, descriptionIndex: Int, valueIndex: Int): ListAppError[Transaction] = {
  //    val c = csvRecordToTransaction(dateIndex, descriptionIndex, valueIndex) _
  //    val t: ListT[IO, AppError[Transaction]] = loadCsvRecords(fileName) map c
  //    val t1: EitherT[IO, NonEmptyList[Errors], List[Transaction]] = EitherT(t.toList() map (_.sequence[AppError, Transaction]))
  //  }


  def createCsv(buckets: Map[String, Double], spend: Map[String, Double], budget: Map[String, Double]) = {
    buckets.toList.filterNot(_._1 == "ignore").foldLeft("") { (acc, e) => acc + s"${e._1}, ${budget.getOrElse(e._1, 0.0)}, ${e._2}, ${-spend.getOrElse(e._1, 0.0)}\n" }
  }

  def loadKeyPairWithNumbers(fileName: String, keyIndex: Int, valueIndex: Int): ScriptOrError[List[(String, Double)]] = {
    def parse(n: String): Option[Double] = {
      n.replace("$", "").replace(",", "").replace("(", "-").replace(")", "").parseDouble.toOption
    }
    scriptListFunctor.map(loadKeyPairs(fileName, keyIndex, valueIndex))(a => a.copy(_2 = parse(a._2).getOrElse(0.0)))
  }

  def loadTransactions(fileName: String, keyIndex: Int, valueIndex: Int): ScriptOrError[List[Transaction]] = {
    scriptListFunctor.map(loadKeyPairWithNumbers(fileName, keyIndex, valueIndex))(pairToTransaction)
  }

  def pairToTransaction(pair: (String, Double)): Transaction = Transaction("", pair._1, pair._2)

  def loadMasterCardCsv(fileName: String): ScriptOrError[List[Transaction]] = loadTransactions(fileName, 2, 3)


  def loadIngDirectCsv(fileName: String): ScriptOrError[List[Transaction]] = {
    scriptListFunctor.map(loadTransactions(fileName, 1, 2))(convertToIng _ andThen negate)
  }

  val atmCharge = """^.*ATM owner fee of \$(\d+\.\d{2}).*""".r

  def convertToIng(transaction: Transaction): Transaction = {
    transaction match {
      case Transaction(_, key, value) => key match {
        case atmCharge(amount) => transaction.copy(amount = value + amount.toDouble)
        case _ => transaction
      }
    }

  }

  def loadQtmbCsv(fileName: String) = scriptListFunctor.map(loadTransactions(fileName, 2, 1))(negate)

  def loadMappings: ScriptOrError[Map[String, String]] = {
    loadKeyPairs("data/Mappings.csv", 0, 1) map (_.toMap.mapValues(_.toLowerCase))
  }


}
