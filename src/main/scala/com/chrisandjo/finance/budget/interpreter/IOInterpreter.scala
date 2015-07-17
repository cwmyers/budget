package com.chrisandjo.finance.budget.interpreter

import java.io.{FileWriter, FileReader}

import com.chrisandjo.finance.budget._
import com.chrisandjo.finance.budget.model.{Description, ReceivedTransaction, Transaction}
import org.apache.commons.csv.{CSVPrinter, CSVRecord, CSVFormat}

import scalaz.effect.IO
import scalaz._, Scalaz._
import scala.collection.JavaConverters._
import io._


object IOInterpreter {

  val ioErrorFunctor = Functor[IO] compose Functor[AppError]
  val ioAppErrorListFunctor = ioErrorFunctor compose Functor[List]


  val interpret = new (AppAction ~> IO) {
    def apply[A](fa: AppAction[A]): IO[A] = fa match {
      case LoadKeyPairs(file, keyIndex, valueIndex) => loadKeyPairs(file, keyIndex, valueIndex)
      case WriteToFile(file, output) => writeToFile(output, file)
      case ReportError(errors) => IO.putStrLn(errors)
      case WriteTransactions(file, transactions) => writeTransactions(file, transactions)
      case LoadTransactions(file) => loadTransactions(file)
      case LoadIngTransactions(file) => loadIngDirectCsv(file)
      case LoadMasterCardTransactions(file) => loadMasterCardCsv(file)
      case LoadQtmbTransactions(file) => loadQtmbCsv(file)
    }
  }


  def loadKeyPairs(fileName: String, keyIndex: Int, valueIndex: Int): IO[AppError[List[(String, String)]]] = {
    val csvs: IO[AppError[List[AppError[(String, String)]]]] = ioAppErrorListFunctor.map(loadCsvRecords(fileName)) {
      r => getRecord(r, keyIndex).map(_.toLowerCase) tuple getRecord(r, valueIndex)
    }
    flatten(csvs)
  }

  def loadCsvRecords(fileName: String): IO[AppError[List[CSVRecord]]] = IO {
    CSVFormat.EXCEL.parse(new FileReader(fileName)).iterator().asScala.toList.tail
  }.catchSomeLeft(e => Some(NonEmptyList(FileNotFound(e))))

  def getRecord(r: CSVRecord, index: Int): AppError[String] = {
    \/.fromTryCatchNonFatal(r.get(index)) leftMap (e => NonEmptyList(BadKeyPair(s"$e $r")))
  }

  def loadIngDirectCsv(fileName: String) = {
    ioAppErrorListFunctor.map(loadReceivedTransactions(fileName, 0, 1, 2))(convertToIng _ andThen CSV.negate)
  }

  def loadQtmbCsv(fileName: String) = {
    ioAppErrorListFunctor.map(loadReceivedTransactions(fileName, 0, 2, 1))(CSV.negate)
  }

  def loadMasterCardCsv(fileName: String) = {
    loadReceivedTransactions(fileName, 1, 2, 3)
  }

  def loadReceivedTransactions(fileName: String, dateIndex: Int, descriptionIndex: Int,
                               amountIndex: Int): IO[AppError[List[ReceivedTransaction]]] = {
    val transactionsWithErrors: IO[AppError[List[AppError[ReceivedTransaction]]]] =
      ioAppErrorListFunctor.map(loadCsvRecords(fileName)) { csvRecord =>
        val record: Int => AppError[String] = getRecord(csvRecord, _: Int)
        val date = record(dateIndex)
        val description = record(descriptionIndex) map (Description)
        val amount = record(amountIndex).flatMap(CSV.parse).getOrElse(0.0).right
        (date |@| description |@| amount)(ReceivedTransaction.apply)
      }
    flatten(transactionsWithErrors)
  }

  val atmCharge = """^.*ATM owner fee of \$(\d+\.\d{2}).*""".r

  def convertToIng(transaction: ReceivedTransaction): ReceivedTransaction = {
    transaction match {
      case ReceivedTransaction(_, key, value) => key.d match {
        case atmCharge(amount) => transaction.copy(amount = value + amount.toDouble)
        case _ => transaction
      }
    }

  }


  def writeTransactions(file: String, transactions: List[Transaction]): IO[AppError[Unit]] = IO {
    val format: CSVFormat = CSVFormat.EXCEL.withHeader("date", "transaction", "amount", "category")
    val writer: FileWriter = new FileWriter(s"$file.csv")
    val csvFilePrinter = new CSVPrinter(writer, format)
    transactions.foreach { transaction =>
      csvFilePrinter.printRecord(transaction.date, transaction.description.d, transaction.amount.toString, transaction.category)
    }
    csvFilePrinter.close()
    writer.close()
  }.catchSomeLeft(e => Some(NonEmptyList(ErrorWritingFile(e))))

  def loadTransactions(file: String): IO[AppError[List[Transaction]]] = {

    val transactionsWithErrors: IO[AppError[List[AppError[Transaction]]]] = ioAppErrorListFunctor.map(loadCsvRecords(s"$file.csv")) { csv =>
      val record: Int => AppError[String] = getRecord(csv, _: Int)
      val date = record(0)
      val description = record(1) map Description
      val amount = record(2).flatMap(CSV.parse).leftMap(e => NonEmptyList(BadNumber(s"Bad number for ${csv.toString}, $e")))
      val category = record(3)
      (date |@| description |@| amount |@| category)(Transaction.apply)
    }
    flatten(transactionsWithErrors)
  }

  def flatten[A](m: IO[AppError[List[AppError[A]]]]): IO[AppError[List[A]]] =
    ioErrorFunctor.map(m)(_.sequence[AppError, A]).map(_.flatMap(identity))


}
