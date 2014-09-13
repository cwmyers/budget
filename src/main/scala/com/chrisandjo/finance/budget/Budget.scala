package com.chrisandjo.finance.budget


import scalaz._, Scalaz._
import scalaz.effect._
import java.io.{File, FileReader}
import org.apache.commons.csv.{CSVRecord, CSVFormat}
import scala.collection.JavaConverters._
import org.apache.commons.io._


object Budget extends SafeApp {

  type AppError[+A] = NonEmptyList[Errors] \/ A
  type VAppError[+A] = Validation[NonEmptyList[Errors], A]

  type ListAppError[+A] = ListT[AppErrorT, A]

  type IOAppError[+A] = ListT[IO, AppError[A]]

  type AppErrorT[+A] = EitherT[IO, NonEmptyList[Errors], A]

  type CSVList = ListT[IO, CSVRecord]

  def loadMasterCardCsv(fileName: String): ListAppError[(String, Double)] = {
    ListT[AppErrorT, (String, Double)](loadKeyPairWithNumbers(fileName, 2, 3))
  }

  def loadQtmbCsv(fileName: String) = {
    ListT[AppErrorT, (String, Double)](loadKeyPairWithNumbers(fileName, 2, 1)) map (a => a.copy(_2 = -a._2))
  }

  def loadCsvRecords(fileName: String): CSVList = {
    ListT.fromList {
      IO {
        CSVFormat.EXCEL.parse(new FileReader(fileName)).iterator().asScala.toList.tail
      }
    }
  }

  def loadMappings = {
    loadKeyPairs("data/Mappings.csv", 0, 1) map (_.toMap.mapValues(_.toLowerCase))
  }

  def loadKeyPairs(fileName: String, keyIndex: Int, valueIndex: Int): AppErrorT[List[(String, String)]] = {
    val map: ListT[IO, AppError[(String, String)]] = loadCsvRecords(fileName).map { r => getRecord(r, keyIndex).map(_.toLowerCase) tuple getRecord(r, valueIndex)}
    EitherT(map.toList() map (_.sequence[AppError, (String, String)]))
  }

  def getRecord(r: CSVRecord, index: Int): AppError[String] = {
    \/.fromTryCatch(r.get(index)) leftMap (e => NonEmptyList(BadKeyPair(s"$e $r")))
  }

  def loadKeyPairWithNumbers(fileName: String, keyIndex: Int, valueIndex: Int): AppErrorT[List[(String, Double)]] = {
    loadKeyPairs(fileName, keyIndex, valueIndex) map (_ map (a => a.copy(_2 = a._2.parseDouble.getOrElse(0.0))))
  }

  def lookup(key: String, m: Map[String, String]): AppError[String] = {
    (m.get(key) orElse m.find {
      case (store, cat) => key.contains(store)
    }.map(_._2)).toRightDisjunction(NonEmptyList(UnknownStore(key)))
  }


  override def runl(args: List[String]): IO[Unit] = {

    val List(budgetFile, masterCardCsv, qtmbCsv, date) = args

    val maybeMappings: AppErrorT[Map[String, String]] = loadMappings

    val maybeBudget: AppErrorT[Map[String, Double]] = loadKeyPairWithNumbers(budgetFile, 0, 1) map (_.toMap)

    val maybeInput: AppErrorT[List[(String, Double)]] = (loadMasterCardCsv(masterCardCsv) |+| loadQtmbCsv(qtmbCsv)).toList()

    val maybeBuckets: AppErrorT[Map[String, Double]] = loadKeyPairWithNumbers(budgetFile, 0, 2) map (_.toMap)

    val reportOrError: AppErrorT[(AppError[String], Map[String, List[(String, Double)]])] = (maybeMappings |@| maybeBudget |@| maybeBuckets |@| maybeInput)(createReports)


    reportOrError.run.flatMap {
      case \/-((csvReport, detailedReport)) => {
        writeToFile(writeDetailedReport(detailedReport), s"detailedReport-$date.txt") |+|
          csvReport.fold(unknown => IO.putStrLn("Unknown Store!\n" + handleFailures(unknown).mkString("\n")),
            writeToFile(_, s"monthlyBudget-$date.csv"))
      }
      case -\/(l) => IO.putStrLn(handleFailures(l).mkString("\n"))
    }

  }


  def writeDetailedReport(report: Map[String, List[(String, Double)]]): String = {
    report.map { case (title, expenses) =>
      s"$title\n======================\n" +
        expenses.mkString("\n")
    }.mkString("\n\n\n") + "\n\n\n"
  }

  def writeToFile(output: String, fileName: String): IO[Unit] = IO {
    FileUtils.writeStringToFile(new File(fileName), output)
  }

  def createReports(mappings: Map[String, String], budget: Map[String, Double], buckets: Map[String, Double], rawSpendings: List[(String, Double)]) = {
    val maybeCategories = (rawSpendings map {
      case (key, value) =>
        lookup(key, mappings) map (a => Map(a -> value))
    }).map(_.validation).sequence[VAppError, Map[String, Double]].disjunction

    val detailedBreakdown = rawSpendings groupBy { case (key, v) => lookup(key, mappings).getOrElse("Unknown")}

    val csvReport = maybeCategories map { categorySpending =>
      val totalSpend = categorySpending.suml.mapValues(-_)
      val newBuckets = buckets mappend (budget mappend totalSpend)
      createCsv(newBuckets, totalSpend, budget)
    }

    (csvReport, detailedBreakdown)
  }

  def createCsv(buckets: Map[String, Double], spend: Map[String, Double], budget: Map[String, Double]) = {
    buckets.toList.filterNot(_._1 == "ignore").foldLeft("") { (acc, e) => acc + s"${e._1}, ${budget.get(e._1).getOrElse(0.0)}, ${e._2}, ${-spend.get(e._1).getOrElse(0.0)}\n"}
  }

  def handleFailures(l: NonEmptyList[Errors]): List[String] = {
    val errors: List[Errors] = l.toList
    errors.collect { case UnknownStore(store) => store}.toSet.toList |+|
      errors.collect { case BadNumber(message) => s"Bad Number: $message"}.toList |+| errors.collect { case BadKeyPair(m) => s"Bad Key Pair: $m"}.toList

  }
}
