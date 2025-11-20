package static

import annotation.tailrec

import ast._
import ast.ParseErrNodes._
import sexprs.SExprs._
import util.ExampleKeyword as Keyword
import util.ExampleKeyword.isKeyword
import util.{NumTypeLit, takeWhileKWPrefixes}
import java.awt.RenderingHints.Key

object Parser:

    /** Parses the given sexpr into a System AST to the best of its ability.
      * If grammar is invalid, an error node is inserted instead.
      *
      * @param sexpr SExpr read in from input
      * @return AST with possible error nodes if grammar rules are violated
      */
    def parseMixedSys(sexpr: SExpr): SystemWE = sexpr match
        //  System ::= (Module^* Import^* Declaration^* Statement^* Expression)
        case SList(Nil) => WE.Err(SystemEmptyList) 
        case SList(elems) => 
            val (modules, rest0) = elems.takeWhileKWPrefixes(Keyword.Module, Keyword.TModule)
            val (imports, rest1) = rest0.takeWhileKWPrefixes(Keyword.Import, Keyword.TImport)
            WE.Node(System(
                modules.map(parseMixedModule),
                imports.map(parseMixedImport),
                parseProgBlock(rest1)
            ))
        case _ => WE.Err(SystemNotAList)

    // Special variant for A9. Only considers typed modules as well-formed
    def parseTypedSys(sexpr: SExpr): SystemWE = sexpr match
        //  TypedSystem ::= (TypedModule^* Import^* Declaration^* Statement^* Expression)
        case SList(Nil) => WE.Err(SystemEmptyList) 
        case SList(elems) => 
            val (tmodules, rest0) = elems.takeWhileKWPrefixes(Keyword.TModule)
            val (imports, rest1) = rest0.takeWhileKWPrefixes(Keyword.Import)
            WE.Node(System(
                tmodules.map(parseMixedModule).map(errIfUntypedMod),
                imports.map(parseMixedImport),
                parseProgBlock(rest1)
            ))
        case _ => WE.Err(SystemNotAList)

    /** Parses the given sexpr into a Class program to the best of its ability.
      * If grammar is invalid, an error node is inserted instead.
      *
      * @param sexpr SExpr read in from input
      * @return Class AST with possible error nodes if grammar rules are violated
      */
    def parseProg(sexpr: SExpr): ProgramWE = sexpr match
        // Program: (Class^* Declaration^* Statement^* Expression)  
        case SList(Nil) => WE.Err(ProgEmptyList) 
        
        case SList(elems) => 
            val (classes, rest) = elems.takeWhileKWPrefixes(Keyword.Class)
            WE.Node(Program(
                classes.map(parseClass),
                parseProgBlock(rest)
            ))

        case _ => WE.Err(ProgNotAList)

    // Module helpers

    def parseMixedModule(sexp: SExpr): ModuleWE = sexp match
        case SList(SSymbol(Keyword.TModule.value) :: moduleName :: rest0) =>
            val (imports, rest1) = rest0.takeWhileKWPrefixes(Keyword.Import, Keyword.TImport)
            rest1 match
                // TModule ::= (tmodule ModuleName MixedImport^* Class Shape)
                case clss :: shape :: Nil => 
                    WE.Node(Module.Typed(
                        parseModuleName(moduleName),
                        imports.map(parseMixedImport),
                        parseClass(clss),
                        parseShape(shape)
                    ))
                case _ => WE.Err(ModuleMalformed) 
        case SList(SSymbol(Keyword.Module.value) :: moduleName :: rest0) =>
            val (imports, rest1) = rest0.takeWhileKWPrefixes(Keyword.Import)
            rest1 match
                // Module  ::= (module ModuleName Import^* Class)
                case clss :: Nil =>
                    WE.Node(Module.Untyped(
                        parseModuleName(moduleName),
                        imports.map(parseUntypedImport),
                        parseClass(clss)
                    ))
                case _ => WE.Err(ModuleMalformed)
        case _ => WE.Err(ModuleMalformed)

    // Special helper for A9. Only considers typed modules as well-formed
    def errIfUntypedMod(mod: ModuleWE) : ModuleWE = mod match
        case WE.Node(Module.Untyped(_, _, _)) => WE.Err(ModuleMalformed) 
        case typedMod @ WE.Node(Module.Typed(_, _, _, _)) => typedMod
        case e => e

    def parseMixedImport(sexp: SExpr): ImportWE = sexp match
        //  Import ::= (import ModuleName)
        case SList(SSymbol(Keyword.Import.value) :: importName :: Nil) =>
            WE.Node(Import.Untyped(parseName(importName)))
        case SList(SSymbol(Keyword.TImport.value) :: importName :: shape :: Nil) =>
            WE.Node(Import.Typed(
                parseName(importName),
                parseShape(shape)
            ))
        case _ =>
            WE.Err(ImportMalformed)
    
    def parseUntypedImport(sexp: SExpr): WE[Import.Untyped[WE]] = sexp match
        case SList(SSymbol(Keyword.Import.value) :: importName :: Nil) =>
            WE.Node(Import.Untyped(parseName(importName)))
        case _ =>
            WE.Err(ImportMalformed)
    

    // Type helpers

    def parseType(sexp: SExpr): TypeWE = sexp match
        case SSymbol(NumTypeLit) => WE.Node(Type.Number())
        case _                   => parseShape(sexp)

    def parseShape(sexp: SExpr): ShapeTypeWE = sexp match
        case SList(SList(fieldTypes) :: SList(methodTypes) :: Nil) =>
            WE.Node(Type.Shape(
                fieldTypes.map(parseFieldType),
                methodTypes.map(parseMethodType),
            ))
        case _ => WE.Err(ShapeMalformed)
    
    def parseFieldType(sexp: SExpr): FieldTypeWE = sexp match
        case SList(fname :: fType :: Nil) => 
            WE.Node(FieldType(
                parseName(fname),
                parseType(fType)
            ))
        case _ => WE.Err(FieldTypeMalformed)
    
    def parseMethodType(sexp: SExpr): MethodTypeWE = sexp match
        case SList(mname :: SList(paramTypes) :: returnType :: Nil) =>
            WE.Node(MethodType(
                parseName(mname),
                paramTypes.map(parseType),
                parseType(returnType)
            ))
        case _ => WE.Err(MethodTypeMalformed)

    // Class helpers

    def parseClass(sexp: SExpr): ClassWE = sexp match
        // Class: (class ClassName (FieldName^*) Method^*)
        case SList(SSymbol(Keyword.Class.value) :: className :: SList(fieldnames) :: methods) =>
            WE.Node(Class(
                parseName(className),
                fieldnames.map(parseName),
                methods.map(parseMethod)
            ))
        case _ => WE.Err(ClassMalformed)

    def parseMethod(sexp: SExpr): MethodWE = sexp match
        // Method: (method MethodName (Parameter^*) Declaration^* Statement^* Expression)
        case SList(SSymbol(Keyword.Method.value) :: methodName :: SList(params) :: rest) =>    
            WE.Node(Method(
                parseName(methodName),
                params.map(parseName),
                parseProgBlock(rest)
            ))
        case _ => WE.Err(MethodMalformed)

    // Core helpers

    def parseProgBlock(sexprs : List[SExpr]) : ProgBlockWE =
        // Program Block: Declaration^* Statement^* Expression
        val (decls,   rest) = sexprs.takeWhileKWPrefixes(Keyword.Def)
        val (stmts,  restE) = (rest.dropRight(1), rest.takeRight(1))
        restE match
            case expr :: Nil => 
                WE.Node(ProgBlock(
                    decls.map(parseDecl),
                    stmts.map(parseStmt),
                    parseExpr(expr)
                ))
            case _ =>  WE.Err(ProgBlockNoExpr)

    def parseDecl(sexp: SExpr): DeclWE = sexp match
        // Declaration: (def Variable Expression)
        case SList(SSymbol(Keyword.Def.value) :: lhs :: rhs :: Nil) =>
            WE.Node(Decl(
                parseName(lhs),
                parseExpr(rhs)
            ))
        case _ => WE.Err(DeclMalformed)

    def parseStmt(sexpr: SExpr): StmtWE = sexpr match
        case SList(SSymbol(Keyword.Def.value) :: _) =>
            WE.Err(DeclAtStmtPosition)

        // Assignment: (Variable = Expression)
        case SList(name :: SSymbol(Keyword.Assign.value) :: expr :: Nil) =>
            WE.Node(Stmt.Assign(
                parseVarRef(name), 
                parseExpr(expr)
            ))
        case SList(name :: SSymbol(Keyword.Assign.value) :: _) =>
            WE.Err(AssignRhsMalformed)
        
        // IfElse: (if0 Expression Block Block)
        case SList(SSymbol(Keyword.If.value) :: grd :: thn :: els :: Nil) =>
            WE.Node(Stmt.Ifelse(
                parseExpr(grd),
                parseBlock(thn),
                parseBlock(els)
            ))
        case SList(SSymbol(Keyword.If.value) :: _) =>
            WE.Err(IfelseMalformed)

        // While: (while0 Expression Block)
        case SList(SSymbol(Keyword.While.value) :: grd :: body :: Nil) =>
            WE.Node(Stmt.While(
                parseExpr(grd),
                parseBlock(body)
            ))
        case SList(SSymbol(Keyword.While.value) :: _) =>
            WE.Err(WhileMalformed)

        // Field Assignment: (Variable --> FieldName = Expression)
        case SList(variable :: SSymbol(Keyword.Accessor.value) :: fieldName :: SSymbol(Keyword.Assign.value) :: expr :: Nil) =>
            WE.Node(Stmt.FieldAssign(
                parseVarRef(variable),
                parseName(fieldName),
                parseExpr(expr)
            ))

        case _ => WE.Err(StmtMalformed)

    def parseBlock(sexpr: SExpr): StmtBlockWE = sexpr match
        // Many: (block Declaration^* Statement^+)
        case SList(SSymbol(Keyword.Block.value) :: Nil) => 
            WE.Err(BlockManyNoStmts)
        case SList(SSymbol(Keyword.Block.value) :: elems) => {
            // Ensures that we have at least one statement
            elems.takeWhileKWPrefixes(Keyword.Def) match
                case (decls, Nil) => 
                    WE.Err(BlockManyNoStmts)
                case (decls, stmts) => 
                    WE.Node(StmtBlock.Many(
                        decls.map(parseDecl),
                        stmts.map(parseStmt)
                    ))
        }
            
        // One: Statement
        case _ => 
            WE.Node(StmtBlock.One(parseStmt(sexpr)))

    def parseExpr(sexpr: SExpr): ExprWE = sexpr match
        // Num: the set of GoodNumbers comprises all inexact numbers
        //      (doubles) between -1000.0 and +1000.0, inclusive.
        case SDouble(n) => 
            WE.Node(Expr.Num(n))

        // Var: the set of Variables consists of all symboSls, minus keywords
        case SSymbol(s) => 
            WE.Node(Expr.Var(parseVarRef(sexpr)))

        //  (new ClassName (Variable^*))
        case SList(SSymbol(Keyword.New.value) :: className :: SList(variables) :: Nil) =>
            WE.Node(Expr.NewInstance(
                parseName(className),
                variables.map(parseVarRef)
            ))
        
        //  (Variable --> FieldName)
        case SList(variable :: SSymbol(Keyword.Accessor.value) :: fieldName :: Nil) =>
            WE.Node(Expr.GetField(parseVarRef(variable), parseName(fieldName)))

        //  (Variable --> MethodName (Variable^*))
        case SList(variable :: SSymbol(Keyword.Accessor.value) :: methodName :: SList(variables) :: Nil) =>
            WE.Node(Expr.CallMethod(
                parseVarRef(variable),
                parseName(methodName),
                variables.map(parseVarRef)
            ))

        //  (Variable isa ClassName)
        case SList(variable :: SSymbol(Keyword.IsA.value) :: className :: Nil) =>
            WE.Node(Expr.IsInstanceOf(
                parseVarRef(variable),
                parseName(className)
            ))

        // Binops
        // Add:    (Variable + Variable)
        // Div:    (Variable / Variable)
        // Equals: (Variable == Variable)
        case SList(sexp1 :: SSymbol(op) :: sexp2 :: Nil) => {
            val (v1 : VarRefWE, v2 : VarRefWE) = (parseVarRef(sexp1), parseVarRef(sexp2))
            op match
                case Keyword.Plus.value     =>  WE.Node(Expr.BinOpExpr(v1, BinOp.Add, v2))
                case Keyword.Div.value      =>  WE.Node(Expr.BinOpExpr(v1, BinOp.Div, v2))
                case Keyword.Eq.value       =>  WE.Node(Expr.BinOpExpr(v1, BinOp.Equals, v2))
                case _                      =>  WE.Err(ExprBadOperand)
        }
        
        case _ =>
            WE.Err(ExprMalformed)

    def parseVarRef(ssymbol : SExpr) : VarRefWE = parseStringWE(ssymbol)
    def parseName(ssymbol : SExpr)   : NameWE = parseStringWE(ssymbol)

    def parseStringWE(ssymbol : SExpr) : WE[String] = 
        ssymbol match
        case SSymbol(s) =>
            if !isKeyword(s) then WE.Node(s)
            else WE.Err(NameIsKeyword) 
        case _ =>
            WE.Err(NotAName)   

    def parseModuleName(ssymbol : SExpr) : NameWE = 
        parseStringWE(ssymbol) match
            case n @ WE.Err(e) => n
            case n @ WE.Node(s) =>
                if s != "Body" then n
                else WE.Err(ModuleNamedBody)
        
        

