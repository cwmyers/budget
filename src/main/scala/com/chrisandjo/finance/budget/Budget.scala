package com.chrisandjo.finance.budget


import scalaz._, Scalaz._
import scalaz.effect._
import java.io.File
import org.apache.commons.io._
import Report._, CSV._


object Budget extends SafeApp {

  def lookup(key: String, m: Map[String, String]): AppError[String] = {
    (m.get(key) orElse m.find {
      case (store, cat) => key.contains(store)
    }.map(_._2)).toRightDisjunction(NonEmptyList(UnknownStore(key)))
  }

  case class Transaction(date: String, transaction: String, amount: Double)


  override def runl(args: List[String]): IO[Unit] = {

    val List(budgetFile, masterCardCsv, qtmbCsv, ingDirectCsv, date) = args

    val maybeMappings: AppErrorT[Map[String, String]] = loadMappings

    val maybeBudget: AppErrorT[Map[String, Double]] = loadKeyPairWithNumbers(budgetFile, 0, 1) map (_.toMap)

    val maybeInput: AppErrorT[List[Transaction]] =
      (loadMasterCardCsv(masterCardCsv) |+|
        loadQtmbCsv(qtmbCsv) |+| loadIngDirectCsv(ingDirectCsv)).toList()

    val maybeBuckets: AppErrorT[Map[String, Double]] = loadKeyPairWithNumbers(budgetFile, 0, 2) map (_.toMap)

    val reportOrError: AppErrorT[(AppError[String], Map[String, List[Transaction]])] =
      (maybeMappings |@| maybeBudget |@| maybeBuckets |@| maybeInput)(createReports)


    reportOrError.run.flatMap {
      case \/-((csvReport, detailedReport)) => {
        writeToFile(writeDetailedReport(detailedReport), s"detailedReport-$date.txt") |+|
          csvReport.fold(unknown => IO.putStrLn("Unknown Store!\n" + handleFailures(unknown).mkString("\n")),
            writeToFile(_, s"monthlyBudget-$date.csv"))
      }
      case -\/(l) => IO.putStrLn(handleFailures(l).mkString("\n"))
    }

  }



  def writeToFile(output: String, fileName: String): IO[Unit] = IO {
    FileUtils.writeStringToFile(new File(fileName), output)
  }

  def createReports(mappings: Map[String, String], budget: Map[String, Double], buckets: Map[String, Double], rawSpendings: List[Transaction]) = {
    val maybeCategories = (rawSpendings map {
      t =>
        lookup(t.transaction, mappings) map (a => Map(a -> t.amount))
    }).map(_.validation).sequence[VAppError, Map[String, Double]].disjunction

    val detailedBreakdown = rawSpendings groupBy { t => lookup(t.transaction, mappings).getOrElse("Unknown")}

    val categorizedSpending = ListT[AppError, Map[String, Double]](maybeCategories)

    //    val totSpend = categorizedSpending.suml
    type y[+A] = ListT[AppError, A]
    val x = Foldable[List]

//    Foldable.suml(totSpend)

    val csvReport = maybeCategories map { categorySpending =>
      val totalSpend = categorySpending.suml.mapValues(-_)
      val newBuckets = buckets |+| budget |+| totalSpend
      createCsv(newBuckets, totalSpend, budget)
    }

    (csvReport, detailedBreakdown)
  }


  def handleFailures(l: NonEmptyList[Errors]): List[String] = {
    val errors: List[Errors] = l.toList
    errors.collect { case UnknownStore(store) => store}.toSet.toList |+|
      errors.collect { case BadNumber(message) => s"Bad Number: $message"}.toList |+| errors.collect { case BadKeyPair(m) => s"Bad Key Pair: $m"}.toList

  }
}
