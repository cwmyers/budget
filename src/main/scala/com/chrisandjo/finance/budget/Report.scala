package com.chrisandjo.finance.budget

import com.chrisandjo.finance.budget.CSV._
import com.chrisandjo.finance.budget.model._

import scalaz.Scalaz._
import scalaz._

object Report {

  def writeDetailedReport(report: Map[String, List[Transaction]]): String = {
    report.map { case (title, expenses) =>
      s"$title\n======================\n" +
        expenses.mkString("\n")
    }.mkString("\n\n\n") + "\n\n\n"
  }

  def createReport(budget: Map[String, Double], buckets: Map[String, Double],
                   transactions: List[Transaction]): String = {
    val totalSpend = transactions.map(t => Map(t.category -> t.amount)).suml.mapValues(-_)
    val newBuckets = buckets |+| budget |+| totalSpend
    createCsv(newBuckets, totalSpend, budget)
  }

  def resolveTransactions(mappings: Map[String, String],
                          rawSpendings: List[ReceivedTransaction]): List[Transaction] =
    rawSpendings map {
      t =>
        Transaction.withCategory(t, lookup(t.transaction, mappings))
    }


  def detailedBreakDown(transactions: List[Transaction]) =
    transactions groupBy { t => t.category }


  def lookup(key: String, m: Map[String, String]): String = {
    (m.get(key) orElse m.find {
      case (store, cat) => key.contains(store)
    }.map(_._2)).getOrElse("unknown")
  }


}
