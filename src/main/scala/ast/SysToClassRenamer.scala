package ast

import static.ModuleDependency
import main.main

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
      case System(modules, imports, progb) =>
        System(
          renameModules(modules, topLevelModule), 
          imports,
          renameProgb(progb, topLevelModule.generateRenameMap())
        )
  
  def renameModules(modules: List[CleanModule], topLevelModule: ModuleDependency): List[CleanModule] =
    modules.map(
      module =>
        topLevelModule.findModuleInDAG(module.mname) match
          case Some(topLevelModuleDep) => 
            renameModule(module, topLevelModuleDep.generateRenameMap())
          case None => 
            throw new Exception(f"Should never happen: the top level module is constructed from a trimmed system module with only reachable modules, so the top level module should always be able to reach this module\nBase Module: ${topLevelModule.dependencies}\nSystem Module:${module.mname}")              
    )

  def renameModule(m: CleanModule, renameMap: Map[String, String]): CleanModule =
    m match
      case Module(mname, imports, clas, shape) =>
        Module(mname, imports, renameClass(clas, renameMap), shape)
    
  def renameClass(c: CleanClass, renameMap: Map[String, String]): CleanClass =
    c match
      case Class(cname, fields, methods) => 
        Class(renameMap(cname), fields, methods.map(renameMethod(_, renameMap)))

  def renameMethod(m: CleanMethod, renameMap: Map[String, String]): CleanMethod =
    m match
      case Method(mname, params, progb) =>
        Method(mname, params, renameProgb(progb, renameMap))

  def renameProgb(p: CleanProgBlock, renameMap: Map[String, String]): CleanProgBlock =
    // println(renameMap)
    p match
      case ProgBlock(decls, stmts, expr) =>
        ProgBlock(
          decls.map(renameDecl(_, renameMap)), 
          stmts.map(renameStmt(_, renameMap)), 
          renameExpr(expr, renameMap))

  def renameDecl(d: CleanDecl, renameMap: Map[String, String]): CleanDecl =
    d match
      case Decl(varDecl, rhs) =>
        Decl(varDecl, renameExpr(rhs, renameMap))

  def renameStmt(s: CleanStmt, renameMap: Map[String, String]): CleanStmt =
    s match
      case Stmt.Assign(lhs, rhs) => 
        Stmt.Assign(lhs, renameExpr(rhs, renameMap))
      case Stmt.FieldAssign(instance, field, rhs) => 
        Stmt.FieldAssign(instance, field, renameExpr(rhs, renameMap))
      case Stmt.Ifelse(guard, tbranch, ebranch) => 
        Stmt.Ifelse(renameExpr(guard, renameMap), renameStmtBlock(tbranch, renameMap), renameStmtBlock(ebranch, renameMap))
      case Stmt.While(guard, body) => 
        Stmt.While(renameExpr(guard, renameMap), renameStmtBlock(body, renameMap))
  
  def renameStmtBlock(stmtBlock: CleanStmtBlock, renameMap: Map[String, String]): CleanStmtBlock =
    stmtBlock match
      case StmtBlock.Many(decls, stmts) =>
        StmtBlock.Many(decls.map(renameDecl(_, renameMap)), stmts.map(renameStmt(_, renameMap)))
      case StmtBlock.One(stmt) => 
        StmtBlock.One(renameStmt(stmt, renameMap))

  def renameExpr(e: CleanExpr, renameMap: Map[String, String]): CleanExpr =
    e match
      case Expr.IsInstanceOf(instance, cname) => 
        Expr.IsInstanceOf(instance, renameMap(cname))
      case Expr.NewInstance(cname, args) =>
        Expr.NewInstance(renameMap(cname), args)
      case _ => e
    
        
    