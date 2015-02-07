package com.chrisandjo.finance.budget

import scalaz.effect.IO
import org.apache.commons.csv.{CSVRecord, CSVFormat}
import java.io.FileReader
import scala.collection.JavaConverters._
import scalaz._, Scalaz._
import com.chrisandjo.finance.budget.Budget.Transaction

object CSV {

  def negate[A](a: (A, Double)): (A, Double) =
    a.copy(_2 = -a._2)

  def negate(a: Transaction): Transaction =
    a.copy(amount = -a.amount)


  def getRecord(r: CSVRecord, index: Int): AppError[String] = {
    \/.fromTryCatch(r.get(index)) leftMap (e => NonEmptyList(BadKeyPair(s"$e $r")))
  }

  def csvRecordToTransaction(dateIndex: Int, descriptionIndex: Int, valueIndex: Int)(r: CSVRecord): AppError[Transaction] = {
    \/.fromTryCatch {
      Transaction(r.get(dateIndex), r.get(descriptionIndex), r.get(valueIndex).toDouble)
    } leftMap (e => NonEmptyList(BadKeyPair(s"$e $r")))
  }


//  def loadTransactions(fileName: String, dateIndex: Int, descriptionIndex: Int, valueIndex: Int): ListAppError[Transaction] = {
//    val c = csvRecordToTransaction(dateIndex, descriptionIndex, valueIndex) _
//    val t: ListT[IO, AppError[Transaction]] = loadCsvRecords(fileName) map c
//    val t1: EitherT[IO, NonEmptyList[Errors], List[Transaction]] = EitherT(t.toList() map (_.sequence[AppError, Transaction]))
//  }

  def loadKeyPairs(fileName: String, keyIndex: Int, valueIndex: Int): AppErrorT[List[(String, String)]] = {
    val map: ListT[IO, AppError[(String, String)]] = loadCsvRecords(fileName).map { r => getRecord(r, keyIndex).map(_.toLowerCase) tuple getRecord(r, valueIndex)}
    EitherT(map.toList() map (_.sequence[AppError, (String, String)]))
  }

  def loadCsvRecords(fileName: String): CSVList = {
    ListT.fromList {
      IO {
        CSVFormat.EXCEL.parse(new FileReader(fileName)).iterator().asScala.toList.tail
      }
    }
  }

  def createCsv(buckets: Map[String, Double], spend: Map[String, Double], budget: Map[String, Double]) = {
    buckets.toList.filterNot(_._1 == "ignore").foldLeft("") { (acc, e) => acc + s"${e._1}, ${budget.get(e._1).getOrElse(0.0)}, ${e._2}, ${-spend.get(e._1).getOrElse(0.0)}\n"}
  }

  def loadKeyPairWithNumbers(fileName: String, keyIndex: Int, valueIndex: Int): AppErrorT[List[(String, Double)]] = {
    loadKeyPairs(fileName, keyIndex, valueIndex) map (_ map (a => a.copy(_2 = a._2.parseDouble.getOrElse(0.0))))
  }

  def loadTransactions(fileName: String, keyIndex: Int, valueIndex: Int): ListAppError[Transaction] = {
    ListT[AppErrorT, Transaction](loadKeyPairWithNumbers(fileName, keyIndex, valueIndex) map (_ map pairToTransaction))
  }

  def pairToTransaction(pair: (String, Double)): Transaction = Transaction("", pair._1, pair._2)

  def loadMasterCardCsv(fileName: String): ListAppError[Transaction] = loadTransactions(fileName, 2, 3)


  def loadIngDirectCsv(fileName: String): ListAppError[Transaction] = {
    loadTransactions(fileName, 1, 2) map (convertToIng _ andThen negate)
  }

  val atmCharge = """^.*ATM owner fee of \$(\d+\.\d{2}).*""".r

  def convertToIng(transaction: Transaction): Transaction = {
    transaction match {
      case Transaction(_, key, value) => key match {
        case atmCharge(amount) => transaction.copy(amount = value + amount.toDouble)
        case _ => transaction
      }
    }

  }

  def loadQtmbCsv(fileName: String) = loadTransactions(fileName, 2, 1) map negate

  def loadMappings = {
    loadKeyPairs("data/Mappings.csv", 0, 1) map (_.toMap.mapValues(_.toLowerCase))
  }


}
