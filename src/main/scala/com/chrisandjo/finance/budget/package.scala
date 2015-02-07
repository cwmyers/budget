package com.chrisandjo.finance

import scalaz._
import scalaz.effect.IO
import org.apache.commons.csv.CSVRecord

package object budget {
  type AppError[+A] = NonEmptyList[Errors] \/ A
  type VAppError[+A] = Validation[NonEmptyList[Errors], A]

  type ListAppError[+A] = ListT[AppErrorT, A]

  type IOAppError[+A] = ListT[IO, AppError[A]]

  type AppErrorT[+A] = EitherT[IO, NonEmptyList[Errors], A]

  type CSVList = ListT[IO, CSVRecord]


}
