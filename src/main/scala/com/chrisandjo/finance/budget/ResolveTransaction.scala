package com.chrisandjo.finance.budget

import com.chrisandjo.finance.budget.AppAction.writeTransactions
import com.chrisandjo.finance.budget.CSV._
import com.chrisandjo.finance.budget.Report._
import scalaz._, Scalaz._

object ResolveTransaction {

  def main(args: Array[String]): Unit = {
    val Array(budgetFile, masterCardCsv, qtmbCsv, ingDirectCsv, date) = args
    val script = for {
      mappings <- loadMappings
      budget <- loadKeyPairWithNumbers(budgetFile, 0, 1) map (_.toMap)
      masterCard <- loadMasterCardCsv(masterCardCsv)
      qtmb <- loadQtmbCsv(qtmbCsv)
      ing <- loadIngDirectCsv(ingDirectCsv)
      transactions = resolveTransactions(mappings, masterCard |+| qtmb |+| ing)
      sortedTransactions = transactions.sortBy(_.category)
      _ <- writeTransactions(s"transactions-$date", sortedTransactions)
    } yield ()

    run(script)

  }

}
