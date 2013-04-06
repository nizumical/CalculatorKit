package com.example.calcbasic

object ModelConfiguration {
  val replaceOperatorIfDuplicated: Boolean = true
}

class GrowingString {
  var storage: List[String] = List[String]()
  def set(text: String) = { storage = text :: storage }
  def concat(additional: String) = { set(current + additional) } // @note no care about the limit
  def undo = { if (storage.nonEmpty) storage = storage.drop(1) }
  def current = if (storage.nonEmpty) storage.head else ""
  def clear = { storage = List[String]() }
  def dump = { println(storage) }
}

class CalcModel {
  val numExpRef = """(-?[0-9]\.[0-9]+)([eE][0-9]+)""".r
  val engine = new CalcEngine
  var inputs = new GrowingString
  var value: Double = 0
  var status: engine.EStatus.Code = engine.EStatus.Incomplete
  var lastOp: String = ""

  private def execute = {
    val result: engine.Result = engine.apply(inputText)
    value = result.evaluatedValue
    status = result.status
    lastOp = result.lastOperator
  }
  def enter(key: String) = {
    inputs.concat(key)
    execute
    if (ModelConfiguration.replaceOperatorIfDuplicated) {
      // Replace last operator if it is duplicated operator
      if (status == engine.EStatus.DupOpError) {
        inputs.undo
        inputs.set(engine.dropLastOperator(inputText) + key)
        execute
      }
    }
  }
  def clear = { inputs.clear; execute }
  def undo = { inputs.undo; execute }
  def inputText = inputs.current
  def statusText = status.toString
  def valueText = value.toString
  def valueTextValidAsInput = value.toString match {
    // Big numbers like 1.23456789123456E10, to be shortened as 1.23456E10
    case numExpRef(numVal, expVal) => {
      val expStr = expVal.toString
      if (expStr.length < engine.numOfDigitsInNumber) {
        numVal.toString.take(engine.numOfDigitsInNumber - expStr.length) + expStr
      } else {
        numVal.toString.take(engine.numOfDigitsInNumber)	// Just like throwing...
      }
    }
    // Normal string like -123.45677890123 to be like -123.45678
    case _ => value.toString.take(engine.numOfDigitsInNumber)
  }
  def isError = engine.EStatus.isError(status)
  def isReady = engine.EStatus.Ready == status
  def isBinaryOperationReady = engine.isBinaryOperationReady(inputText)
}

