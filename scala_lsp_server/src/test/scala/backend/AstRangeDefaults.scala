package test.backend

import ast.*
import sexprs.{Position, Range}

object AstRangeDefaults:
  val DummyRange = Range(Position(0, 0), Position(0, 0))

  def stripRanges(raw: RawSystemWE): RawSystemWE =
    raw match
      case WE.Node(rawSystem) => WE.Node(stripRawSystemWE(rawSystem))
      case err: WE.Err => err

  def stripRanges(system: SystemWE): SystemWE =
    system match
      case WE.Node(sys) => WE.Node(stripSystemWE(sys))
      case err: WE.Err => err

  def stripRanges(program: ProgramWE): ProgramWE =
    program match
      case WE.Node(prog) => WE.Node(stripProgramWE(prog))
      case err: WE.Err => err

  def stripRanges(raw: CleanRawSystem): CleanRawSystem =
    stripRawSystemClean(raw)

  def stripRanges(system: CleanSystem): CleanSystem =
    stripSystemClean(system)

  def stripRanges(program: CleanProgram): CleanProgram =
    stripProgramClean(program)

  private def stripRawSystemWE(raw: RawSystem[WE]): RawSystem[WE] =
    RawSystem(
      modules = raw.modules.map(stripModuleWE),
      imports = raw.imports.map(stripImportWE),
      progb = stripProgBlockWE(raw.progb),
      range = DummyRange
    )

  private def stripSystemWE(system: System[WE]): System[WE] =
    System(
      modules = system.modules.map(stripModuleWE),
      imports = system.imports.map(stripImportWE),
      progb = stripProgBlockWE(system.progb),
      moddata = system.moddata,
      range = DummyRange
    )

  private def stripProgramWE(program: Program[WE]): Program[WE] =
    Program(
      clss = program.clss.map(stripClassWE),
      progb = stripProgBlockWE(program.progb),
      range = DummyRange
    )

  private def stripModuleWE(module: WE[Module[WE]]): WE[Module[WE]] =
    module match
      case WE.Node(Module(mname, imports, clas, _)) =>
        WE.Node(Module(mname, imports.map(stripImportWE), stripClassWE(clas), DummyRange))
      case err: WE.Err => err

  private def stripImportWE(imported: WE[Import[WE]]): WE[Import[WE]] =
    imported match
      case WE.Node(Import.Untyped(mname, _)) =>
        WE.Node(Import.Untyped(mname, DummyRange))
      case WE.Node(Import.Typed(mname, shape, _)) =>
        WE.Node(Import.Typed(mname, stripShapeWE(shape), DummyRange))
      case err: WE.Err => err

  private def stripClassWE(classNode: WE[Class[WE]]): WE[Class[WE]] =
    classNode match
      case WE.Node(Class(cname, fields, methods, shape, _)) =>
        WE.Node(
          Class(
            cname,
            fields,
            methods.map(stripMethodWE),
            shape.map(stripShapeWE),
            DummyRange
          )
        )
      case err: WE.Err => err

  private def stripMethodWE(method: WE[Method[WE]]): WE[Method[WE]] =
    method match
      case WE.Node(Method(mname, params, progb, _)) =>
        WE.Node(Method(mname, params, stripProgBlockWE(progb), DummyRange))
      case err: WE.Err => err

  private def stripProgBlockWE(block: WE[ProgBlock[WE]]): WE[ProgBlock[WE]] =
    block match
      case WE.Node(ProgBlock(decls, stmts, expr, _)) =>
        WE.Node(
          ProgBlock(
            decls.map(stripDeclWE),
            stmts.map(stripStmtWE),
            stripExprWE(expr),
            DummyRange
          )
        )
      case err: WE.Err => err

  private def stripDeclWE(decl: WE[Decl[WE]]): WE[Decl[WE]] =
    decl match
      case WE.Node(Decl(varDecl, rhs, _)) =>
        WE.Node(Decl(varDecl, stripExprWE(rhs), DummyRange))
      case err: WE.Err => err

  private def stripStmtWE(stmt: WE[Stmt[WE]]): WE[Stmt[WE]] =
    stmt match
      case WE.Node(Stmt.Assign(lhs, rhs, _)) =>
        WE.Node(Stmt.Assign(lhs, stripExprWE(rhs), DummyRange))
      case WE.Node(Stmt.Ifelse(guard, tbranch, ebranch, _)) =>
        WE.Node(
          Stmt.Ifelse(
            stripExprWE(guard),
            stripStmtBlockWE(tbranch),
            stripStmtBlockWE(ebranch),
            DummyRange
          )
        )
      case WE.Node(Stmt.While(guard, body, _)) =>
        WE.Node(Stmt.While(stripExprWE(guard), stripStmtBlockWE(body), DummyRange))
      case WE.Node(Stmt.FieldAssign(instance, field, rhs, _)) =>
        WE.Node(Stmt.FieldAssign(instance, field, stripExprWE(rhs), DummyRange))
      case err: WE.Err => err

  private def stripStmtBlockWE(block: WE[StmtBlock[WE]]): WE[StmtBlock[WE]] =
    block match
      case WE.Node(StmtBlock.One(stmt, _)) =>
        WE.Node(StmtBlock.One(stripStmtWE(stmt), DummyRange))
      case WE.Node(StmtBlock.Many(decls, stmts, _)) =>
        WE.Node(
          StmtBlock.Many(
            decls.map(stripDeclWE),
            stmts.map(stripStmtWE),
            DummyRange
          )
        )
      case err: WE.Err => err

  private def stripExprWE(expr: WE[Expr[WE]]): WE[Expr[WE]] =
    expr match
      case WE.Node(Expr.Num(n, _)) =>
        WE.Node(Expr.Num(n, DummyRange))
      case WE.Node(Expr.Var(x, _)) =>
        WE.Node(Expr.Var(x, DummyRange))
      case WE.Node(Expr.BinOpExpr(lhs, op, rhs, _)) =>
        WE.Node(Expr.BinOpExpr(lhs, op, rhs, DummyRange))
      case WE.Node(Expr.NewInstance(cname, args, _)) =>
        WE.Node(Expr.NewInstance(cname, args, DummyRange))
      case WE.Node(Expr.GetField(instance, field, _)) =>
        WE.Node(Expr.GetField(instance, field, DummyRange))
      case WE.Node(Expr.CallMethod(instance, method, args, _)) =>
        WE.Node(Expr.CallMethod(instance, method, args, DummyRange))
      case WE.Node(Expr.IsInstanceOf(instance, cname, _)) =>
        WE.Node(Expr.IsInstanceOf(instance, cname, DummyRange))
      case err: WE.Err => err

  private def stripShapeWE(shape: WE[Type.Shape[WE]]): WE[Type.Shape[WE]] =
    shape match
      case WE.Node(Type.Shape(fieldTypes, methodTypes, _)) =>
        WE.Node(
          Type.Shape(
            fieldTypes.map(stripFieldTypeWE),
            methodTypes.map(stripMethodTypeWE),
            DummyRange
          )
        )
      case err: WE.Err => err

  private def stripTypeWE(tpe: WE[Type[WE]]): WE[Type[WE]] =
    tpe match
      case WE.Node(Type.Number(_)) => WE.Node(Type.Number(DummyRange))
      case WE.Node(Type.Shape(fieldTypes, methodTypes, _)) =>
        WE.Node(
          Type.Shape(
            fieldTypes.map(stripFieldTypeWE),
            methodTypes.map(stripMethodTypeWE),
            DummyRange
          )
        )
      case err: WE.Err => err

  private def stripFieldTypeWE(fieldType: WE[FieldType[WE]]): WE[FieldType[WE]] =
    fieldType match
      case WE.Node(FieldType(fname, ftype, _)) =>
        WE.Node(FieldType(fname, stripTypeWE(ftype), DummyRange))
      case err: WE.Err => err

  private def stripMethodTypeWE(methodType: WE[MethodType[WE]]): WE[MethodType[WE]] =
    methodType match
      case WE.Node(MethodType(mname, paramTypes, returnType, _)) =>
        WE.Node(
          MethodType(
            mname,
            paramTypes.map(stripTypeWE),
            stripTypeWE(returnType),
            DummyRange
          )
        )
      case err: WE.Err => err

  private def stripRawSystemClean(raw: RawSystem[Clean]): RawSystem[Clean] =
    RawSystem(
      modules = raw.modules.map(stripModuleClean),
      imports = raw.imports.map(stripImportClean),
      progb = stripProgBlockClean(raw.progb),
      range = DummyRange
    )

  private def stripSystemClean(system: System[Clean]): System[Clean] =
    System(
      modules = system.modules.map(stripModuleClean),
      imports = system.imports.map(stripImportClean),
      progb = stripProgBlockClean(system.progb),
      moddata = system.moddata,
      range = DummyRange
    )

  private def stripProgramClean(program: Program[Clean]): Program[Clean] =
    Program(
      clss = program.clss.map(stripClassClean),
      progb = stripProgBlockClean(program.progb),
      range = DummyRange
    )

  private def stripModuleClean(module: Module[Clean]): Module[Clean] =
    Module(
      module.mname,
      module.imports.map(stripImportClean),
      stripClassClean(module.clas),
      DummyRange
    )

  private def stripImportClean(imported: Import[Clean]): Import[Clean] =
    imported match
      case Import.Untyped(mname, _) =>
        Import.Untyped(mname, DummyRange)
      case Import.Typed(mname, shape, _) =>
        Import.Typed(mname, stripShapeClean(shape), DummyRange)

  private def stripClassClean(classNode: Class[Clean]): Class[Clean] =
    Class(
      classNode.cname,
      classNode.fields,
      classNode.methods.map(stripMethodClean),
      classNode.shape.map(stripShapeClean),
      DummyRange
    )

  private def stripMethodClean(method: Method[Clean]): Method[Clean] =
    Method(
      method.mname,
      method.params,
      stripProgBlockClean(method.progb),
      DummyRange
    )

  private def stripProgBlockClean(block: ProgBlock[Clean]): ProgBlock[Clean] =
    ProgBlock(
      block.decls.map(stripDeclClean),
      block.stmts.map(stripStmtClean),
      stripExprClean(block.expr),
      DummyRange
    )

  private def stripDeclClean(decl: Decl[Clean]): Decl[Clean] =
    Decl(decl.varDecl, stripExprClean(decl.rhs), DummyRange)

  private def stripStmtClean(stmt: Stmt[Clean]): Stmt[Clean] =
    stmt match
      case Stmt.Assign(lhs, rhs, _) =>
        Stmt.Assign(lhs, stripExprClean(rhs), DummyRange)
      case Stmt.Ifelse(guard, tbranch, ebranch, _) =>
        Stmt.Ifelse(
          stripExprClean(guard),
          stripStmtBlockClean(tbranch),
          stripStmtBlockClean(ebranch),
          DummyRange
        )
      case Stmt.While(guard, body, _) =>
        Stmt.While(stripExprClean(guard), stripStmtBlockClean(body), DummyRange)
      case Stmt.FieldAssign(instance, field, rhs, _) =>
        Stmt.FieldAssign(instance, field, stripExprClean(rhs), DummyRange)

  private def stripStmtBlockClean(block: StmtBlock[Clean]): StmtBlock[Clean] =
    block match
      case StmtBlock.One(stmt, _) =>
        StmtBlock.One(stripStmtClean(stmt), DummyRange)
      case StmtBlock.Many(decls, stmts, _) =>
        StmtBlock.Many(
          decls.map(stripDeclClean),
          stmts.map(stripStmtClean),
          DummyRange
        )

  private def stripExprClean(expr: Expr[Clean]): Expr[Clean] =
    expr match
      case Expr.Num(n, _) =>
        Expr.Num(n, DummyRange)
      case Expr.Var(x, _) =>
        Expr.Var(x, DummyRange)
      case Expr.BinOpExpr(lhs, op, rhs, _) =>
        Expr.BinOpExpr(lhs, op, rhs, DummyRange)
      case Expr.NewInstance(cname, args, _) =>
        Expr.NewInstance(cname, args, DummyRange)
      case Expr.GetField(instance, field, _) =>
        Expr.GetField(instance, field, DummyRange)
      case Expr.CallMethod(instance, method, args, _) =>
        Expr.CallMethod(instance, method, args, DummyRange)
      case Expr.IsInstanceOf(instance, cname, _) =>
        Expr.IsInstanceOf(instance, cname, DummyRange)

  private def stripShapeClean(shape: Type.Shape[Clean]): Type.Shape[Clean] =
    Type.Shape(
      shape.fieldTypes.map(stripFieldTypeClean),
      shape.methodTypes.map(stripMethodTypeClean),
      DummyRange
    )

  private def stripTypeClean(tpe: Type[Clean]): Type[Clean] =
    tpe match
      case Type.Number(_) => Type.Number(DummyRange)
      case Type.Shape(fieldTypes, methodTypes, _) =>
        Type.Shape(
          fieldTypes.map(stripFieldTypeClean),
          methodTypes.map(stripMethodTypeClean),
          DummyRange
        )

  private def stripFieldTypeClean(fieldType: FieldType[Clean]): FieldType[Clean] =
    FieldType(fieldType.fname, stripTypeClean(fieldType.fieldType), DummyRange)

  private def stripMethodTypeClean(methodType: MethodType[Clean]): MethodType[Clean] =
    MethodType(
      methodType.mname,
      methodType.paramTypes.map(stripTypeClean),
      stripTypeClean(methodType.returnType),
      DummyRange
    )
