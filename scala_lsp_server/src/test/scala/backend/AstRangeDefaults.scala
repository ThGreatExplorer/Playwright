package test.backend

import ast._
import sexprs.{Position, Range}

object AstRangeDefaults:
  val DummyRange = Range(Position(0, 0), Position(0, 0))

  extension (raw: RawSystem.type)
    def apply[Node[_]](
        modules: List[Node[Module[Node]]],
        imports: List[Node[Import[Node]]],
        progb: Node[ProgBlock[Node]]
    ): RawSystem[Node] =
      RawSystem(modules, imports, progb, DummyRange)

  extension (sys: System.type)
    def apply[Node[_]](
        modules: List[Node[Module[Node]]],
        imports: List[Node[Import[Node]]],
        progb: Node[ProgBlock[Node]],
        moddata: ModuleData
    ): System[Node] =
      System(modules, imports, progb, moddata, DummyRange)

  extension (mod: Module.type)
    def apply[Node[_]](
        mname: Node[Name],
        imports: List[Node[Import[Node]]],
        clas: Node[Class[Node]]
    ): Module[Node] =
      Module(mname, imports, clas, DummyRange)

  extension (imp: Import.Untyped.type)
    def apply[Node[_]](mname: Node[Name]): Import.Untyped[Node] =
      Import.Untyped(mname, DummyRange)

  extension (imp: Import.Typed.type)
    def apply[Node[_]](mname: Node[Name], shape: Node[Type.Shape[Node]]): Import.Typed[Node] =
      Import.Typed(mname, shape, DummyRange)

  extension (prog: Program.type)
    def apply[Node[_]](
        clss: List[Node[Class[Node]]],
        progb: Node[ProgBlock[Node]]
    ): Program[Node] =
      Program(clss, progb, DummyRange)

  extension (cls: Class.type)
    def apply[Node[_]](
        cname: Node[Name],
        fields: List[Node[Name]],
        methods: List[Node[Method[Node]]],
        shape: Option[Node[Type.Shape[Node]]]
    ): Class[Node] =
      Class(cname, fields, methods, shape, DummyRange)

  extension (m: Method.type)
    def apply[Node[_]](
        mname: Node[Name],
        params: List[Node[Name]],
        progb: Node[ProgBlock[Node]]
    ): Method[Node] =
      Method(mname, params, progb, DummyRange)

  extension (pb: ProgBlock.type)
    def apply[Node[_]](
        decls: List[Node[Decl[Node]]],
        stmts: List[Node[Stmt[Node]]],
        expr: Node[Expr[Node]]
    ): ProgBlock[Node] =
      ProgBlock(decls, stmts, expr, DummyRange)

  extension (decl: Decl.type)
    def apply[Node[_]](varDecl: Node[Name], rhs: Node[Expr[Node]]): Decl[Node] =
      Decl(varDecl, rhs, DummyRange)

  extension (stmt: Stmt.Assign.type)
    def apply[Node[_]](lhs: Node[VarRef], rhs: Node[Expr[Node]]): Stmt.Assign[Node] =
      Stmt.Assign(lhs, rhs, DummyRange)

  extension (stmt: Stmt.Ifelse.type)
    def apply[Node[_]](
        guard: Node[Expr[Node]],
        tbranch: Node[StmtBlock[Node]],
        ebranch: Node[StmtBlock[Node]]
    ): Stmt.Ifelse[Node] =
      Stmt.Ifelse(guard, tbranch, ebranch, DummyRange)

  extension (stmt: Stmt.While.type)
    def apply[Node[_]](guard: Node[Expr[Node]], body: Node[StmtBlock[Node]]): Stmt.While[Node] =
      Stmt.While(guard, body, DummyRange)

  extension (stmt: Stmt.FieldAssign.type)
    def apply[Node[_]](
        instance: Node[VarRef],
        field: Node[Name],
        rhs: Node[Expr[Node]]
    ): Stmt.FieldAssign[Node] =
      Stmt.FieldAssign(instance, field, rhs, DummyRange)

  extension (block: StmtBlock.One.type)
    def apply[Node[_]](stmt: Node[Stmt[Node]]): StmtBlock.One[Node] =
      StmtBlock.One(stmt, DummyRange)

  extension (block: StmtBlock.Many.type)
    def apply[Node[_]](
        decls: List[Node[Decl[Node]]],
        stmts: List[Node[Stmt[Node]]]
    ): StmtBlock.Many[Node] =
      StmtBlock.Many(decls, stmts, DummyRange)

  extension (expr: Expr.Num.type)
    def apply[Node[_]](n: NumVal): Expr.Num[Node] =
      Expr.Num(n, DummyRange)

  extension (expr: Expr.Var.type)
    def apply[Node[_]](x: Node[VarRef]): Expr.Var[Node] =
      Expr.Var(x, DummyRange)

  extension (expr: Expr.BinOpExpr.type)
    def apply[Node[_]](lhs: Node[VarRef], op: BinOp, rhs: Node[VarRef]): Expr.BinOpExpr[Node] =
      Expr.BinOpExpr(lhs, op, rhs, DummyRange)

  extension (expr: Expr.NewInstance.type)
    def apply[Node[_]](cname: Node[Name], args: List[Node[VarRef]]): Expr.NewInstance[Node] =
      Expr.NewInstance(cname, args, DummyRange)

  extension (expr: Expr.GetField.type)
    def apply[Node[_]](instance: Node[VarRef], field: Node[Name]): Expr.GetField[Node] =
      Expr.GetField(instance, field, DummyRange)

  extension (expr: Expr.CallMethod.type)
    def apply[Node[_]](
        instance: Node[VarRef],
        method: Node[Name],
        args: List[Node[VarRef]]
    ): Expr.CallMethod[Node] =
      Expr.CallMethod(instance, method, args, DummyRange)

  extension (expr: Expr.IsInstanceOf.type)
    def apply[Node[_]](instance: Node[VarRef], cname: Node[Name]): Expr.IsInstanceOf[Node] =
      Expr.IsInstanceOf(instance, cname, DummyRange)

  extension (t: Type.Number.type)
    def apply[Node[_]](): Type.Number[Node] =
      Type.Number(DummyRange)

  extension (t: Type.Shape.type)
    def apply[Node[_]](
        fieldTypes: List[Node[FieldType[Node]]],
        methodTypes: List[Node[MethodType[Node]]]
    ): Type.Shape[Node] =
      Type.Shape(fieldTypes, methodTypes, DummyRange)

  extension (t: FieldType.type)
    def apply[Node[_]](fname: Node[Name], fieldType: Node[Type[Node]]): FieldType[Node] =
      FieldType(fname, fieldType, DummyRange)

  extension (t: MethodType.type)
    def apply[Node[_]](
        mname: Node[Name],
        paramTypes: List[Node[Type[Node]]],
        returnType: Node[Type[Node]]
    ): MethodType[Node] =
      MethodType(mname, paramTypes, returnType, DummyRange)
