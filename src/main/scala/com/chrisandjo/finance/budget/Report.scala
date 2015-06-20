package com.chrisandjo.finance.budget

import com.chrisandjo.finance.budget.Budget.Transaction
import com.chrisandjo.finance.budget.CSV._

import scalaz._,Scalaz._

object Report {

  def writeDetailedReport(report: Map[String, List[Transaction]]): String = {
    report.map { case (title, expenses) =>
      s"$title\n======================\n" +
        expenses.mkString("\n")
    }.mkString("\n\n\n") + "\n\n\n"
  }

  def createReport(mappings: Map[String, String], budget: Map[String, Double], buckets: Map[String, Double], rawSpendings: List[Transaction]):AppError[String] = {
    val maybeCategories = (rawSpendings map {
      t =>
        lookup(t.transaction, mappings) map (a => Map(a -> t.amount))
    }).map(_.validation).sequence[VAppError, Map[String, Double]].disjunction


    maybeCategories map { categorySpending =>
      val totalSpend = categorySpending.suml.mapValues(-_)
      val newBuckets = buckets |+| budget |+| totalSpend
      createCsv(newBuckets, totalSpend, budget)
    }
  }

  def detailedBreakDown(transactions: List[Transaction], mappings: Map[String, String]) =
    transactions groupBy { t => lookup(t.transaction, mappings).getOrElse("Unknown") }


  def lookup(key: String, m: Map[String, String]): AppError[String] = {
    (m.get(key) orElse m.find {
      case (store, cat) => key.contains(store)
    }.map(_._2)).toRightDisjunction(NonEmptyList(UnknownStore(key)))
  }



}
