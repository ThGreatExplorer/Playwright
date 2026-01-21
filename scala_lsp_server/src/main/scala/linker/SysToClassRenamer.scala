package linker

import ast._
import static.ModuleData

object SystemToClassRenamerAST:

  /**
    * Renames the classes for the given Clean System. It takes the system with the
    * corresponding base node representing the Module Dependency Graph.
    *
    * @param s clean system
    * @param topLevelModule root node of the Module Dependency graph associated 
    * with the system
    * @return the renamed System
    */
  def renameSystem(s: CleanSystem, topLevelModule: ModuleDependency): CleanSystem =
    s match
      case System(modules, imports, progb, _, range) =>
        val renamedModules = renameModules(modules, topLevelModule)
        
        System(
          renamedModules, 
          imports,
          renameProgb(progb, topLevelModule.generateRenameMap()), 
          ModuleData(renamedModules),
          range
        )
  
  def renameModules(modules: List[CleanModule], topLevelModule: ModuleDependency): List[CleanModule] =
    modules.map(
      module =>
        module match
          case Module(mname, imports, clas, _) => 
            topLevelModule.findModuleInDAG(mname) match
              case Some(topLevelModuleDep) => 
                renameModule(module, topLevelModuleDep.generateRenameMap())
              case None => 
                throw new Exception(f"Should never happen: the top level module is constructed from a trimmed system module with only reachable modules, so the top level module should always be able to reach this module\nBase Module: ${topLevelModule.dependencies}\nSystem Module:${mname}")              
    )

  def renameModule(m: CleanModule, renameMap: Map[String, String]): CleanModule =
    m match
      case Module(mname, imports, clas, range) =>
        Module(mname, imports, renameClass(clas, renameMap), range)
    
  def renameClass(c: CleanClass, renameMap: Map[String, String]): CleanClass =
    c match
      case Class(cname, fields, methods, shape, range) => 
        Class(renameMap(cname), fields, methods.map(renameMethod(_, renameMap)), shape, range)

  def renameMethod(m: CleanMethod, renameMap: Map[String, String]): CleanMethod =
    m match
      case Method(mname, params, progb, range) =>
        Method(mname, params, renameProgb(progb, renameMap), range)

  def renameProgb(p: CleanProgBlock, renameMap: Map[String, String]): CleanProgBlock =
    // println(renameMap)
    p match
      case ProgBlock(decls, stmts, expr, range) =>
        ProgBlock(
          decls.map(renameDecl(_, renameMap)), 
          stmts.map(renameStmt(_, renameMap)), 
          renameExpr(expr, renameMap),
          range)

  def renameDecl(d: CleanDecl, renameMap: Map[String, String]): CleanDecl =
    d match
      case Decl(varDecl, rhs, range) =>
        Decl(varDecl, renameExpr(rhs, renameMap), range)

  def renameStmt(s: CleanStmt, renameMap: Map[String, String]): CleanStmt =
    s match
      case Stmt.Assign(lhs, rhs, range) => 
        Stmt.Assign(lhs, renameExpr(rhs, renameMap), range)
      case Stmt.FieldAssign(instance, field, rhs, range) => 
        Stmt.FieldAssign(instance, field, renameExpr(rhs, renameMap), range)
      case Stmt.Ifelse(guard, tbranch, ebranch, range) => 
        Stmt.Ifelse(
          renameExpr(guard, renameMap),
          renameStmtBlock(tbranch, renameMap),
          renameStmtBlock(ebranch, renameMap),
          range
        )
      case Stmt.While(guard, body, range) => 
        Stmt.While(renameExpr(guard, renameMap), renameStmtBlock(body, renameMap), range)
  
  def renameStmtBlock(stmtBlock: CleanStmtBlock, renameMap: Map[String, String]): CleanStmtBlock =
    stmtBlock match
      case StmtBlock.Many(decls, stmts, range) =>
        StmtBlock.Many(
          decls.map(renameDecl(_, renameMap)),
          stmts.map(renameStmt(_, renameMap)),
          range
        )
      case StmtBlock.One(stmt, range) => 
        StmtBlock.One(renameStmt(stmt, renameMap), range)

  def renameExpr(e: CleanExpr, renameMap: Map[String, String]): CleanExpr =
    e match
      case Expr.IsInstanceOf(instance, cname, range) => 
        Expr.IsInstanceOf(instance, renameMap(cname), range)
      case Expr.NewInstance(cname, args, range) =>
        Expr.NewInstance(renameMap(cname), args, range)
      case _ => e
    
        
    
