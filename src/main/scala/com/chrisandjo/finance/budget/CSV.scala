package com.chrisandjo.finance.budget

import com.chrisandjo.finance.budget.AppAction.{Script, loadKeyPairs}
import com.chrisandjo.finance.budget.model.{Description, ReceivedTransaction}
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

  def negate(a: ReceivedTransaction): ReceivedTransaction =
    a.copy(amount = -a.amount)


  def csvRecordToTransaction(dateIndex: Int, descriptionIndex: Int, valueIndex: Int)(r: CSVRecord): AppError[ReceivedTransaction] = {
    \/.fromTryCatchNonFatal {
      ReceivedTransaction(r.get(dateIndex), Description(r.get(descriptionIndex)), r.get(valueIndex).toDouble)
    } leftMap (e => NonEmptyList(BadKeyPair(s"$e $r")))
  }


  def createCsv(buckets: Map[String, Double], spend: Map[String, Double], budget: Map[String, Double]) = {
    buckets.toList.filterNot(_._1 == "ignore").foldLeft("") { (acc, e) => acc + s"${e._1}, ${budget.getOrElse(e._1, 0.0)}, ${e._2}, ${-spend.getOrElse(e._1, 0.0)}\n" }
  }

  def parse(n: String): Throwable \/ Double = {
    n.replace("$", "").replace(",", "").replace("(", "-").replace(")", "").parseDouble.disjunction
  }

  def loadKeyPairWithNumbers(fileName: String, keyIndex: Int, valueIndex: Int): ScriptOrError[List[(String, Double)]] = {
    scriptListFunctor.map(loadKeyPairs(fileName, keyIndex, valueIndex))(a => a.copy(_2 = parse(a._2).getOrElse(0.0)))
  }

  def loadTransactions(fileName: String, keyIndex: Int, valueIndex: Int): ScriptOrError[List[ReceivedTransaction]] = {
    scriptListFunctor.map(loadKeyPairWithNumbers(fileName, keyIndex, valueIndex))(pairToTransaction)
  }

  def pairToTransaction(pair: (String, Double)): ReceivedTransaction = ReceivedTransaction("", Description(pair._1), pair._2)


  def loadMappings: ScriptOrError[Map[String, String]] = {
    loadKeyPairs("data/Mappings.csv", 0, 1) map (_.toMap.mapValues(_.toLowerCase))
  }


}
