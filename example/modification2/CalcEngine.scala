package com.example.calcbasic

import scala.util.control.Breaks.{break,breakable}

/**
 * User: Daisuke
 * Date: 2013/04/01
 * Time: 23:20
 */

class CalcEngine {
  // Token definitions
  private val NumberRef   = """(-?[0-9]+([\.][0-9]*([eE][0-9]+)?)?)(.*)""".r
  private val PlusRef     = """([+])(.*)""".r
  private val MinusRef    = """([-])(.*)""".r
  private val MultiplyRef = """([\*x])(.*)""".r
  private val DivideRef   = """([/])(.*)""".r
  private val AndRef      = """([&])(.*)""".r
  // Limit definitions
  val numOfDigitsInNumber = 10

  // Engine status
  object EStatus extends Enumeration {
    type Code = Value
    val Incomplete, Ready, ParseError, SyntaxError, Div0Error, DupOpError = Value
    def isError(code: Code) = code match {
      case Incomplete|Ready => false
      case _                => true
    }
  }
  // Token type to distinguish tokens by type
  object ETokenType extends Enumeration {
    type Name = Value
    val Number, Operator = Value
    def isNumber(name: Name) = name == Number
    def isOperator(name: Name) = name == Operator
  }

  case class ParseResult(status: EStatus.Code, list: List[Token])
  case class Token(tokenType: ETokenType.Name, word: String)
  class Result(val evaluatedValue: Double,
                     val status: EStatus.Code,
                      val lastOperator: String)

  def parse(input: String, tokens: List[Token]): ParseResult = {
    def isAcceptableMinus = tokens.length match {
      case 0 => true
      case 1 => ETokenType.isNumber(tokens.last.tokenType)
      case _ if ETokenType.isNumber(tokens.last.tokenType) || ETokenType.isNumber(tokens(tokens.length - 2).tokenType) => true
      case _ => false
    }
    def isPreviousNumber = tokens.nonEmpty && ETokenType.isNumber(tokens.last.tokenType)
    input match {
      case MinusRef(op, rest) if (isPreviousNumber) => parse(rest, tokens :+ new Token(ETokenType.Operator, op))
      case NumberRef(num, _, _, rest) if (num.length <= numOfDigitsInNumber) => {
        parse(rest, tokens :+ new Token(ETokenType.Number, num))
      }
      case PlusRef(op, rest)       => parse(rest, tokens :+ new Token(ETokenType.Operator, op))
      case MinusRef(op, rest) if (isAcceptableMinus) => parse(rest, tokens :+ new Token(ETokenType.Operator, op))
      case MultiplyRef(op, rest)   => parse(rest, tokens :+ new Token(ETokenType.Operator, op))
      case DivideRef(op, rest)     => parse(rest, tokens :+ new Token(ETokenType.Operator, op))
      case AndRef(op, rest)        => parse(rest, tokens :+ new Token(ETokenType.Operator, op))
      case ""                      => ParseResult(EStatus.Ready, tokens)
      case _                       => ParseResult(EStatus.ParseError, List[Token]())
    }
  }

  def isOperator(input: String) = input match {
    case PlusRef(op, rest)     => true
    case MinusRef(op, rest)    => true
    case MultiplyRef(op, rest) => true
    case DivideRef(op, rest)   => true
    case AndRef(op, rest)   => true
    case _ => false
  }

  private def arithmeticOp(op: String, num1: Double, num2: Double): (Double, EStatus.Code) = op match {
    // Error case
    case "/" if (num2 == 0) => (num1, EStatus.Incomplete)  // just leave it as incomplete, not as Div0Error
    // Successful case
    case "-"     => (num1 - num2, EStatus.Ready)
    case "+"     => (num1 + num2, EStatus.Ready)
    case "*"|"x" => (num1 * num2, EStatus.Ready)
    case "/"     => (num1 / num2, EStatus.Ready)
    case "&"     => (num1.toInt & num2.toInt, EStatus.Ready)
  }

  def evaluate(tokens: List[Token]): Result = {
    var state: String = "waiting"   // Initial state of evaluator state machine
    var value: Double = 0
    var evalStatus= EStatus.Incomplete
    var lastOp: String = ""
    // Run evaluator state machine
    breakable(for (cur <- tokens) {
      state match {
        case "waiting" if (ETokenType.isNumber(cur.tokenType))        => { state = "numberReady"
          evalStatus = EStatus.Ready; value = cur.word.toDouble }
        case "numberReady" if (ETokenType.isOperator(cur.tokenType)) => { state = "waitingNext"
          evalStatus = EStatus.Incomplete; lastOp = cur.word }
        case "waitingNext" if (ETokenType.isNumber(cur.tokenType))   => { state = "numberReady"
          val (v, status) = arithmeticOp(lastOp, value, cur.word.toDouble)
          evalStatus = status; value = v }
        // Error case
        case "waitingNext" if (ETokenType.isOperator(cur.tokenType)) => { evalStatus = EStatus.DupOpError }
        case "waiting"|"waitingNext" if (cur.word == "-")            => { evalStatus = EStatus.Incomplete }
        case _ => { evalStatus = EStatus.SyntaxError }
      }
      if (EStatus.isError(evalStatus)) break()
    })
    new Result(value, evalStatus, lastOp)
  }

  def apply(input: String): Result = {
    // parse input
    val tokens = parse(input, List[Token]())
    // evaluate parsed calculation
    tokens.status match {
      case EStatus.Ready => evaluate(tokens.list)
      case _             => new Result(0, tokens.status, "")
    }
  }

  def dropLastOperator(input: String): String = {
    // Get list of tokens
    var list = parse(input, List[Token]()).list
    if (!ETokenType.isOperator(list.last.tokenType)) return input
    // Drop last operator
    list = list.dropRight(1)
    // Concatenate rest to return
    var result = ""
    list.foreach(w => result += w.word)
    result
  }

  def isBinaryOperationReady(input: String): Boolean = {
    val parseResult = parse(input, List[Token]())
    val list = parseResult.list
    parseResult.status == EStatus.Ready &&
      list.length == 3 &&
      ETokenType.isNumber(list(0).tokenType) &&
      ETokenType.isOperator(list(1).tokenType) &&
      ETokenType.isNumber(list(2).tokenType)
  }
}




