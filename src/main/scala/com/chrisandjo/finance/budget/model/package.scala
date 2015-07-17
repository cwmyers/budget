package com.chrisandjo.finance.budget


package object model {
  case class Description(d: String) extends AnyVal
  case class ReceivedTransaction(date: String, description: Description, amount: Double)
  case class Transaction(date: String, description: Description, amount: Double, category: String)

  object Transaction {
    def withCategory(r: ReceivedTransaction, category: String) =
      Transaction(r.date, r.description,r.amount, category)
  }



}
