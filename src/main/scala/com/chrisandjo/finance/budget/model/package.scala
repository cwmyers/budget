package com.chrisandjo.finance.budget


package object model {

  case class ReceivedTransaction(date: String, transaction: String, amount: Double)
  case class Transaction(date: String, transaction: String, amount: Double, category: String)

  object Transaction {
    def withCategory(r: ReceivedTransaction, category: String) =
      Transaction(r.date, r.transaction,r.amount, category)
  }



}
