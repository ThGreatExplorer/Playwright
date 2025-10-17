package main

import sexprs.SExprs._
import ast.ConverterToClean.progToClean
import ast.{NumVal, ProgramWE, CleanProgram}
import static.Parser
import static.{VCheckClassDups, VCheckMethodFieldParamDups, VCheckUndefined}
// import cesk.{CESKMachine, RuntimeError}

enum Result:
  case Count(n : Int)
  case ParseError
  case ParseBelongs
  case DupClassDefs
  case DupMethodFieldParams
  case UndefinedVarError
  case ValidityBelongs
  case Success(n : Number)
  case RuntimeError

  def outputString: String = this match
    case Count(n) => s"\"$n\""
    case ParseError => "\"parser error\""
    case ParseBelongs => "\"belongs\""

    case DupClassDefs => "\"duplicate class name\""
    case DupMethodFieldParams => "\"duplicate method, field, or parameter name\""
    case UndefinedVarError => "\"undeclared variable error\""
    case ValidityBelongs => "\"belongs\""

    case Success(n) => s"$n"
    case RuntimeError => "\"run-time error\""

object AssignmentRunner:

  def resOrClean(errRes : Result, prog : ProgramWE) : Either[Result, CleanProgram] =
    progToClean(prog) match 
      case None            => Left[Result, CleanProgram](errRes)
      case Some(cleanProg) => Right[Result, CleanProgram](cleanProg)

  def unpackResult(realRes : Either[Result, CleanProgram], resOnSucc : Result) : Result =
    realRes match 
      case Left(errRes)    => errRes
      case Right(_)        => resOnSucc

  /**
    * Result printer for Assignment 6 — Class: Syntax
    * 
    * @param input SExpr read from stdin
    */
  def classParseAndValidity(input: SExpr): Result =

    val pipeRes = 
      for 
        parsedProg     <- resOrClean(Result.ParseError, Parser.parseProg(input))
        progNoDupClass <- resOrClean(Result.DupClassDefs, VCheckClassDups.classDupsProg(parsedProg))
        progNoDupMFPs  <- resOrClean(Result.DupMethodFieldParams, VCheckMethodFieldParamDups.mfpDupsProg(progNoDupClass))
        validProg      <- resOrClean(Result.UndefinedVarError, VCheckUndefined.closedProg(progNoDupMFPs))
      yield
        validProg
    
    unpackResult(pipeRes, Result.ValidityBelongs)

  /**
    * Result printer for Assignment 5 — Core: CESK
    * 
    * @param input SExpr read from stdin
    */
  // def ceskCore(input: SExpr): Result =
  //   val parsedProg = Parser.parseProg(input)

  //   progToClean(parsedProg) match
  //     case None => Result.ParseError
  //     case Some(cleanProg) => 
  //       val validatedProg = ValidityChecker.closedProg(cleanProg)
        
  //       progToClean(validatedProg) match 
  //         case None    => Result.UndefinedVarError
  //         case Some(validProg) => 
  //           CESKMachine.run(validProg) match
  //             case n : NumVal => Result.Success(n)
  //             case e : RuntimeError => Result.RuntimeError

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
        val validatedProg = VCheckUndefined.closedProg(cleanProg)
        
        progToClean(validatedProg) match 
          case None    => Result.UndefinedVarError
          case Some(_) => Result.ValidityBelongs

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