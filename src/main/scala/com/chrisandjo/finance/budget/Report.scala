package com.chrisandjo.finance.budget

import com.chrisandjo.finance.budget.Budget.Transaction

object Report {

  def writeDetailedReport(report: Map[String, List[Transaction]]): String = {
    report.map { case (title, expenses) =>
      s"$title\n======================\n" +
        expenses.mkString("\n")
    }.mkString("\n\n\n") + "\n\n\n"
  }


}
