package com.chrisandjo.finance.budget


import com.chrisandjo.finance.budget.AppAction._
import com.chrisandjo.finance.budget.CSV._
import com.chrisandjo.finance.budget.Report._


object Budget {

  def main(args: Array[String]): Unit = {
    val Array(budgetFile, masterCardCsv, qtmbCsv, ingDirectCsv, date) = args
    val script = for {
      mappings <- loadMappings
      budget <- loadKeyPairWithNumbers(budgetFile, 0, 1) map (_.toMap)
      buckets <- loadKeyPairWithNumbers(budgetFile, 0, 2) map (_.toMap)
      transactions <- loadResolvedTransactions(s"transactions-$date")
      detailedReport = detailedBreakDown(transactions)
      _ <- writeToFile(s"detailedReport-$date.txt", writeDetailedReport(detailedReport))
      report = createReport(budget, buckets, transactions)
      _ <- writeToFile(s"monthlyBudget-$date.csv", report)
    } yield ()

    run(script)
  }



}
