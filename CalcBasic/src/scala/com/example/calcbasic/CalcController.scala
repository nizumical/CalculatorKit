package com.example.calcbasic

class CalcController {
  private val model = new CalcModel
  def enter(key: String) = model.enter(key)
  def undo = model.undo
  def clear = model.clear
  def execute() = model.execute
  def getValueText = model.valueText
  def getValueTextValidAsInput = model.valueTextValidAsInput
  def getInputText = model.inputText
  def getStatusText = model.statusText
  def isError = model.isError
  def isReady = model.isReady
  def isBinaryOperationReady = model.isBinaryOperationReady
  def dump = println("input=" + model.inputText + " => value=" + model.valueText + " (" + model.statusText
                     + (if (model.isBinaryOperationReady) "for binary op" else "")  + ") "
                     + getValueTextValidAsInput)
}

