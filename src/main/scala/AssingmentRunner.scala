package main

import sexprs.SExprs._
import static.ValidityChecker
import ast.ConverterToClean.progToClean
import ast.NumVal
import static.Parser
import cesk.{CESKMachine, RuntimeError}

enum Result:
  case Count(n : Int)
  case ParseError
  case ParseBelongs
  case UndefinedVarError
  case ValidityBelongs
  case Success(n : Number)
  case RuntimeError

  def outputString: String = this match
    case Count(n) => s"\"$n\""
    case ParseError => "\"parser error\""
    case ParseBelongs => "\"belongs\""
    case UndefinedVarError => "\"undeclared variable error\""
    case ValidityBelongs => "\"belongs\""
    case Success(n) => s"$n"
    case RuntimeError => "\"run-time error\""

object AssignmentRunner:

  /**
    * Result printer for Assignment 5 — Core: CESK
    * 
    * @param input SExpr read from stdin
    */
  def ceskCore(input: SExpr): Result =
    val parsedProg = Parser.parseProg(input)

    progToClean(parsedProg) match
      case None => Result.ParseError
      case Some(cleanProg) => 
        val validatedProg = ValidityChecker.closedProg(cleanProg)
        
        progToClean(validatedProg) match 
          case None    => Result.UndefinedVarError
          case Some(validProg) => 
            CESKMachine.run(validProg) match
              case n : NumVal => Result.Success(n)
              case e : RuntimeError => Result.RuntimeError

  /**
    * Result printer for Assignment 4 — Core: Validity
    * 
    * @param input SExpr read from stdin
    */
  def coreValidityChecker(input: SExpr): Result =
    val parsedProg = Parser.parseProg(input)

    progToClean(parsedProg) match
      case None => Result.ParseError
      case Some(cleanProg) => 
        val validatedProg = ValidityChecker.closedProg(cleanProg)
        
        progToClean(validatedProg) match 
          case None    => Result.UndefinedVarError
          case Some(_) => Result.ValidityBelongs
  // TODO: Look into using Either[Result, CleanProgram]

  // /**
  //   * Result printer for Assignment 3 — Bare Bones: CSK
  //   *
  //   * @param input SExpr read from stdin
  //   */
  // def cskBareBones(input: SExpr): Result = 
  //   val prog = Parser.parseProg(input)

  //   if progHasError(prog) then
  //     Result.ParseError
  //   else
  //     CSKMachine.run(prog) match
  //       case n: Number => Result.Success(n)
  //       case _ => Result.RuntimeError

  /**
    * Result printer for Assignment 2 — Bare Bones: Parser
    *
    * @param input SExpr read from stdin
    */
  def parserBareBones(input: SExpr): Result = 
    val parsedProg = Parser.parseProg(input)

    progToClean(parsedProg) match
      case None => 
        Result.ParseError
      case Some(_) => 
        Result.ParseBelongs
      

  /**
    * Result printer for Assignment 1 — Start Me Up
    *
    * @param input SExpr read from stdin
    */
  def startUp(input: SExpr): Result =     
    Result.Count(counter(input))
