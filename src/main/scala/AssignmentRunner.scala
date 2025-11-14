package main

import sexprs.SExprs._
import util.UnreachablePatternMatch
import ast.ConverterToClean.{progToClean, systemToClean}
import ast.{NumVal, ProgramWE, CleanProgram, CleanSystem, SystemWE}
import static.Parser.{parseProg, parseMixedSys, parseTypedSys}
import static.VCheckTLDups.{classDupsProg, moduleDupsSys}
import static.VCheckMFPNameDups.{mfpDupsProg, mfpDupsSys}
import static.VCheckImports.{checkImportsSys}
import static.VCheckUndefined.{closedProg, closedSystem}
import static.Typechecker.{typecheckSystem}
import linker.SystemToClassLinker.{linkProgram}
import cesk.{CESKMachine, RuntimeError, ObjectVal}

enum Result:
  case Count(n : Int)
  case ParseError
  case ParseBelongs
  case DupClassDefs
  case DupModuleDefs
  case DupMethodFieldParams
  case BadImport
  case UndefinedVarError
  case TypeError
  case ValidityBelongs
  case SuccObj
  case SuccNum(n : Number)
  case RuntimeError

  def outputString: String = this match
    case Count(n) => s"\"$n\""
    case ParseError => "\"parser error\""
    case ParseBelongs => "\"belongs\""

    case DupClassDefs => "\"duplicate class name\""
    case DupModuleDefs => "\"duplicate module name\""
    case DupMethodFieldParams => "\"duplicate method, field, or parameter name\""
    case BadImport => "\"bad import\""
    case UndefinedVarError => "\"undeclared variable error\""
    case TypeError => "\"type error\""
    case ValidityBelongs => "\"belongs\""

    case SuccObj => "\"object\""
    case SuccNum(n) => s"$n"
    case RuntimeError => "\"run-time error\""

object AssignmentRunner: 

  /**
    * Result printer for Assignment 10 — A La JS
    * 
    * @param input SExpr read from stdin
    */
  def mixedSystem(input: SExpr): Result =

    val pipeRes = 
      for 
        parsedSys      <- resOrCleanSys(Result.ParseError,           parseMixedSys(input))
        sysNoDupMods   <- resOrCleanSys(Result.DupModuleDefs,        moduleDupsSys(parsedSys))
        sysNoDupMFPs   <- resOrCleanSys(Result.DupMethodFieldParams, mfpDupsSys(sysNoDupMods))
        sysGoodImps    <- resOrCleanSys(Result.BadImport,            checkImportsSys(sysNoDupMFPs))
        validSys       <- resOrCleanSys(Result.UndefinedVarError,    closedSystem(sysGoodImps))
        typecheckedSys <- resOrCleanSys(Result.TypeError,            typecheckSystem(validSys))
      yield
        typecheckedSys
    
    pipeRes match 
      case Left(errRes)     => errRes
      case Right(validSys) => 
        val classProg = linkProgram(validSys)
        CESKMachine(classProg).run match
          case n: NumVal       => Result.SuccNum(n)
          case e: RuntimeError => Result.RuntimeError
          // Our typechecker does not allow this, but after introducing mixed modules, it is 
          // possible to violate this constraint
          case o: ObjectVal    => Result.SuccObj 

  /**
    * Result printer for Assignment 9 — Static Types
    * 
    * @param input SExpr read from stdin
    */
  def typedSystem(input: SExpr): Result =

    val pipeRes = 
      for 
        parsedSys      <- resOrCleanSys(Result.ParseError,           parseTypedSys(input))
        sysNoDupMods   <- resOrCleanSys(Result.DupModuleDefs,        moduleDupsSys(parsedSys))
        sysNoDupMFPs   <- resOrCleanSys(Result.DupMethodFieldParams, mfpDupsSys(sysNoDupMods))
        validSys       <- resOrCleanSys(Result.UndefinedVarError,    closedSystem(sysNoDupMFPs))
        typecheckedSys <- resOrCleanSys(Result.TypeError,            typecheckSystem(validSys))
      yield
        typecheckedSys
    
    pipeRes match 
      case Left(errRes)     => errRes
      case Right(validSys) => 
        val classProg = linkProgram(validSys)
        CESKMachine(classProg).run match
          case n: NumVal       => Result.SuccNum(n)
          case e: RuntimeError => Result.RuntimeError
          case o: ObjectVal    => 
            throw new UnreachablePatternMatch(
              "Should never happen: typed system returns object"
            )   
          
  /**
    * Result printer for Assignment 8 — Modules
    * 
    * @param input SExpr read from stdin
    */
  def ceskModule(input: SExpr): Result =

    val pipeRes = 
      for 
        parsedSys     <- resOrCleanSys(Result.ParseError,           parseMixedSys(input))
        sysNoDupMods  <- resOrCleanSys(Result.DupModuleDefs,        moduleDupsSys(parsedSys))
        sysNoDupMFPs  <- resOrCleanSys(Result.DupMethodFieldParams, mfpDupsSys(sysNoDupMods))
        validSys      <- resOrCleanSys(Result.UndefinedVarError,    closedSystem(sysNoDupMFPs))
      yield
        validSys
    
    pipeRes match 
      case Left(errRes)     => errRes
      case Right(validSys) => 
        val classProg = linkProgram(validSys)
        CESKMachine(classProg).run match
          case n: NumVal    => Result.SuccNum(n)
          case o: ObjectVal => Result.SuccObj
          case _            => Result.RuntimeError

  /**
    * Result printer for Assignment 7 — Class: Semantics
    * 
    * @param input SExpr read from stdin
    */
  def ceskClass(input: SExpr): Result =

    val pipeRes = 
      for 
        parsedProg     <- resOrCleanProg(Result.ParseError,           parseProg(input))
        progNoDupClass <- resOrCleanProg(Result.DupClassDefs,         classDupsProg(parsedProg))
        progNoDupMFPs  <- resOrCleanProg(Result.DupMethodFieldParams, mfpDupsProg(progNoDupClass))
        validProg      <- resOrCleanProg(Result.UndefinedVarError,    closedProg(progNoDupMFPs))
      yield
        validProg
    
    pipeRes match 
      case Left(errRes)     => errRes
      case Right(validProg) => 
        CESKMachine(validProg).run match
          case n: NumVal    => Result.SuccNum(n)
          case o: ObjectVal => Result.SuccObj
          case _            => Result.RuntimeError

  /**
    * Result printer for Assignment 6 — Class: Syntax
    * 
    * @param input SExpr read from stdin
    */
  def classParseAndValidity(input: SExpr): Result =

    val pipeRes = 
      for 
        parsedProg     <- resOrCleanProg(Result.ParseError,           parseProg(input))
        progNoDupClass <- resOrCleanProg(Result.DupClassDefs,         classDupsProg(parsedProg))
        progNoDupMFPs  <- resOrCleanProg(Result.DupMethodFieldParams, mfpDupsProg(progNoDupClass))
        validProg      <- resOrCleanProg(Result.UndefinedVarError,    closedProg(progNoDupMFPs))
      yield
        validProg
    
    pipeRes match 
      case Left(errRes)    => errRes
      case Right(_)        => Result.ValidityBelongs

  /**
    * Result printer for Assignment 5 — Core: CESK
    * 
    * @param input SExpr read from stdin
    */
  def ceskCore(input: SExpr): Result =
    val parsedProg = parseProg(input)

    progToClean(parsedProg) match
      case None => Result.ParseError
      case Some(cleanProg) => 
        val validatedProg = closedProg(cleanProg)
        
        progToClean(validatedProg) match 
          case None    => Result.UndefinedVarError
          case Some(validProg) => 
            CESKMachine(validProg).run match
              case n : NumVal => Result.SuccNum(n)
              case o : ObjectVal => Result.SuccObj
              case e : RuntimeError => Result.RuntimeError

  /**
    * Result printer for Assignment 4 — Core: Validity
    * 
    * @param input SExpr read from stdin
    */
  def coreValidityChecker(input: SExpr): Result =
    val parsedProg = parseProg(input)

    progToClean(parsedProg) match
      case None => Result.ParseError
      case Some(cleanProg) => 
        val validatedProg = closedProg(cleanProg)
        
        progToClean(validatedProg) match 
          case None    => Result.UndefinedVarError
          case Some(_) => Result.ValidityBelongs

  //////////////
  // DOES NOT WORK DUE TO CESK MACHINE EXPECTING VAR DECLS
  //////////////
  // /**
  //   * Result printer for Assignment 3 — Bare Bones: CSK
  //   *
  //   * @param input SExpr read from stdin
  //   */
  // def cskBareBones(input: SExpr): Result = 
  //   val prog = Parser.parseProg(input)
  //   progToClean(prog) match
  //     case None => Result.ParseError
  //     case Some(cleanProg) => 
  //       CESKMachine(cleanProg).run match
  //         case n: NumVal => Result.SuccNum(n)
  //         case _ => Result.RuntimeError
  //////////////

  /**
    * Result printer for Assignment 2 — Bare Bones: Parser
    *
    * @param input SExpr read from stdin
    */
  def parserBareBones(input: SExpr): Result = 
    val parsedProg = parseProg(input)

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

  /******************************************************************************
    Helpers for building the pipeline
  *****************************************************************************/

  def resOrCleanSys(errRes : Result, sys : SystemWE) : Either[Result, CleanSystem] =
    systemToClean(sys) match   
      case None            => 
        Left [Result, CleanSystem](errRes)
      case Some(cleanSys) => Right[Result, CleanSystem](cleanSys)

  def resOrCleanProg(errRes : Result, prog : ProgramWE) : Either[Result, CleanProgram] =
    progToClean(prog) match   
      case None            => 
        Left [Result, CleanProgram](errRes)
      case Some(cleanProg) => Right[Result, CleanProgram](cleanProg)
