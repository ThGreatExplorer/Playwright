package main

import sexprs.SExprs._
import csk.CSKMachine
import csk.Control

import parser.Parser as ParserAST

enum Result:
  case Count(n : Int)
  case ParseError
  case ParseBelongs
  case Success(n : Number)
  case RuntimeError

  def outputString: String = this match
    case Count(n) => s"\"$n\""
    case ParseError => "\"parser error\""
    case ParseBelongs => "\"belongs\""
    case Success(n) => s"$n"
    case RuntimeError => "\"runtime error\""

object AssignmentRunner:
  /**
    * Result printer for Assignment 3 — Bare Bones: CSK
    *
    * @param input SExpr read from stdin
    */
  def cskBareBones(input: SExpr): Result = 
    val prog = ParserAST.parse(input)

    if ParserAST.hasError(prog) then
      Result.ParseError
    else
      CSKMachine.run(prog) match
        case n: Number => Result.Success(n)
        case _ => Result.RuntimeError

  /**
    * Result printer for Assignment 2 — Bare Bones: Parser
    *
    * @param input SExpr read from stdin
    */
  def parserBareBones(input: SExpr): Result = 
    val prog = ParserAST.parse(input)

    if ParserAST.hasError(prog) then
      Result.ParseError
    else
      Result.ParseBelongs

  /**
    * Result printer for Assignment 1 — Start Me Up
    *
    * @param input SExpr read from stdin
    */
  def startUp(input: SExpr): Result =     
    Result.Count(counter(input))
