package com.chrisandjo.finance.budget

import java.io.File

import org.apache.commons.io.FileUtils

import scalaz.NonEmptyList
import scalaz.effect.IO

package object io {

  def writeToFile(output: String, fileName: String): IO[AppError[Unit]] = IO {
    FileUtils.writeStringToFile(new File(fileName), output)
  }.catchSomeLeft(e => Some(NonEmptyList(ErrorWritingFile(e))))

}
