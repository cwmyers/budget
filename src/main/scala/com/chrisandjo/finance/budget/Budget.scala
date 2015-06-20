package com.chrisandjo.finance.budget


import com.chrisandjo.finance.budget.AppAction._
import com.chrisandjo.finance.budget.CSV._
import com.chrisandjo.finance.budget.Report._
import com.chrisandjo.finance.budget.interpreter.IOInterpreter

import scalaz.Scalaz._
import scalaz._
import scalaz.effect._


object Budget extends SafeApp {



  case class Transaction(date: String, transaction: String, amount: Double)


  override def runl(args: List[String]): IO[Unit] = {

    val List(budgetFile, masterCardCsv, qtmbCsv, ingDirectCsv, date) = args


    val script = for {
      mappings <- loadMappings
      budget <- loadKeyPairWithNumbers(budgetFile, 0, 1) map (_.toMap)
      masterCard <- loadMasterCardCsv(masterCardCsv)
      qtmb <- loadQtmbCsv(qtmbCsv)
      ing <- loadIngDirectCsv(ingDirectCsv)
      transactions = masterCard |+| qtmb |+| ing
      buckets <- loadKeyPairWithNumbers(budgetFile, 0, 2) map (_.toMap)
      detailedReport = detailedBreakDown(transactions, mappings)
      _ <- writeToFile(s"detailedReport-$date.txt", writeDetailedReport(detailedReport))
      report <- createReport(mappings, budget, buckets, transactions).lift
      _ <- writeToFile(s"monthlyBudget-$date.csv",report)
    } yield ()

    Free.runFC(resolveErrors(script))(IOInterpreter.interpret)
  }


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
