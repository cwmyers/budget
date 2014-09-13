package com.chrisandjo.finance.budget


import scalaz._, Scalaz._
import scalaz.effect._
import java.io.FileReader
import org.apache.commons.csv.{CSVRecord, CSVFormat}
import scala.collection.JavaConverters._


object Budget extends SafeApp {

  type AppError[+A] = NonEmptyList[Errors] \/ A

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

  def lookup(key: String, m: Map[String, String]) = {
    m.get(key) orElse m.find { case (store, cat) => key.contains(store)}.map(_._2) getOrElse "Unknown"
  }


  override def runl(args: List[String]): IO[Unit] = {

    val maybeMappings: AppErrorT[Map[String, String]] = loadMappings

    val maybeBudget: AppErrorT[Map[String, Double]] = loadKeyPairWithNumbers("data/Budget.csv", 0, 1) map (_.toMap)

    val maybeInput: AppErrorT[List[(String, Double)]] = (loadMasterCardCsv("data/Data-4.CSV") |+| loadQtmbCsv("data/CURRENTACCOUNT.csv")).toList()

    val maybeBuckets: AppErrorT[Map[String, Double]] = loadKeyPairWithNumbers("data/Buckets.csv", 0, 1) map (_.toMap)



    val value1: AppErrorT[String] = (maybeMappings |@| maybeBudget |@| maybeBuckets |@| maybeInput) {
      (mappings: Map[String, String], budget: Map[String, Double], buckets: Map[String, Double], rawSpendings: List[(String, Double)]) => {
        val categories = rawSpendings map {
          case (key, value) =>
            Map(lookup(key, mappings) -> value)
        }
        val totalSpend = categories.suml.mapValues(-_)
        val result = buckets mappend (budget mappend totalSpend)
        createCsv(result, totalSpend, budget)
      }
    }
    value1.run.flatMap {
      case \/-(result) => IO.putStrLn(result)
      case -\/(l) => IO.putStrLn(handleFailures(l).mkString("\n"))
    }


    //    def lookup(key: String): AppErrorT[String] = {
    //      mappings flatMap { m: Map[String, String] =>
    //        val value: \/[NonEmptyList[UnknownStore], String] = (m.get(key) orElse m.find { case (store, cat) => key.contains(store)}.map(_._2)).toRightDisjunction(NonEmptyList(UnknownStore(key)))
    //        EitherT(IO(value))
    //      }
    //    }


    //    val categories: ListAppError[Map[String, Double]] = {
    //      val map: ListT[AppErrorT, EitherT[IO, NonEmptyList[Errors], Map[String, Double]]] = input.map {
    //        case (key, value) => {
    //          lookup(key) map (a => Map(a -> value))
    //        }
    //      }
    //      map
    //    }

    //    val totalSpend = categories.suml.mapValues(-_)
    //
    //    val res = (totalSpend |@| maybeBuckets |@| maybeBudget) {
    //      (expenses, buckets, budgets) => buckets mappend (budgets mappend expenses)
    //    }

    //    IO {
    //      res tuple totalSpend tuple maybeBudget match {
    //        case -\/(l) => handleFailures(l).foreach((s: String) => println(s))
    //        case \/-(((r, ts), budget)) => {
    //          println("BUCKETS")
    //          println(createCsv(r, ts, budget))
    //        }
    //      }
    //    }

  }


  def createCsv(buckets: Map[String, Double], spend: Map[String, Double], budget: Map[String, Double]) = {
    buckets.toList.foldLeft("") { (acc, e) => acc + s"${e._1}, ${budget.get(e._1).getOrElse(0.0)}, ${e._2}, ${-spend.get(e._1).getOrElse(0.0)}\n"}
  }

  def handleFailures(l: NonEmptyList[Errors]): List[String] = {
    val errors: List[Errors] = l.toList
    errors.collect { case UnknownStore(store) => store}.toSet.toList |+|
      errors.collect { case BadNumber(message) => s"Bad Number: $message"}.toList |+| errors.collect { case BadKeyPair(m) => s"Bad Key Pair: $m"}.toList

  }
}
