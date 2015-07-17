package com.chrisandjo.finance.budget

import com.chrisandjo.finance.budget.AppAction._
import com.chrisandjo.finance.budget.Report._
import scalaz._, Scalaz._

object ResolveTransaction {

  def main(args: Array[String]): Unit = {
    val Array(budgetFile, masterCardCsv, qtmbCsv, ingDirectCsv, date) = args
    val script = for {
      mappings <- CSV.loadMappings
      budget <- CSV.loadKeyPairWithNumbers(budgetFile, 0, 1) map (_.toMap)
      masterCard <- loadMasterCardTransactions(masterCardCsv)
      qtmb <- loadQTMBTransactions(qtmbCsv)
      ing <- loadIngTransactions(ingDirectCsv)
      transactions = resolveTransactions(mappings, masterCard |+| qtmb |+| ing)
      sortedTransactions = transactions.sortBy(_.category)
      _ <- writeTransactions(s"transactions-$date", sortedTransactions)
    } yield ()

    run(script)

  }

}
