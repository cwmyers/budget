package com.chrisandjo.finance.budget

sealed trait Errors
case class UnknownStore(store:String) extends Errors
case class BadNumber(message:String) extends Errors
case class BadKeyPair(message:String) extends Errors