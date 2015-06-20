package com.chrisandjo.finance.budget.interpreter

import java.io.FileReader

import com.chrisandjo.finance.budget._
import org.apache.commons.csv.{CSVRecord, CSVFormat}

import scalaz.effect.IO
import scalaz._, Scalaz._
import scala.collection.JavaConverters._
import io._


object IOInterpreter {

  val ioAppErrorListFunctor = Functor[IO] compose Functor[AppError] compose Functor[List]
  val ioErrorFunctor = Functor[IO] compose Functor[AppError]
  

  val interpret = new (AppAction ~> IO) {
    override def apply[A](fa: AppAction[A]): IO[A] = fa match {
      case LoadKeyPairs(file, keyIndex, valueIndex) => loadKeyPairs(file, keyIndex, valueIndex)
      case WriteToFile(file, output) => writeToFile(output, file)
      case ReportError(errors) => IO.putStrLn(errors)

    }
  }


  def loadKeyPairs(fileName: String, keyIndex: Int, valueIndex: Int): IO[AppError[List[(String, String)]]] = {
    val csvs:IO[AppError[List[AppError[(String,String)]]]] = ioAppErrorListFunctor.map(loadCsvRecords(fileName)){ r => getRecord(r, keyIndex).map(_.toLowerCase) tuple getRecord(r, valueIndex)}
    val map: IO[AppError[AppError[List[(String, String)]]]] = ioErrorFunctor.map(csvs)(a => a.sequence[AppError,(String,String)])
    map.map(_.flatMap(identity))
  }

  def loadCsvRecords(fileName: String): IO[AppError[List[CSVRecord]]] = {
      IO {
        CSVFormat.EXCEL.parse(new FileReader(fileName)).iterator().asScala.toList.tail
      }.catchSomeLeft(e => Some(NonEmptyList(FileNotFound(e))))
  }

  def getRecord(r: CSVRecord, index: Int): AppError[String] = {
    \/.fromTryCatchNonFatal(r.get(index)) leftMap (e => NonEmptyList(BadKeyPair(s"$e $r")))
  }

}
