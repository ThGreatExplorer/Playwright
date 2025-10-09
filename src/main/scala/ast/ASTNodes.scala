/******************************************************************************
  This file defines our AST data represetnation.

  Each node type has two versions:
    - NameWE: represents an AST node that can potentically include error nodes
      in its chidren nodes or be an error node itself.
      (WE stands for "with error")
    - CleanName: represents an AST node that does not have error nodes in
      any of its children and is itself a valid node.  

 *****************************************************************************/

package ast

// Program    ::= (Declaration^* Statement^* Expression)
enum ProgramWE:
    case Prog(decls: List[DeclWE], stmts: List[StmtWE], expr: ExprWE)
    case Err(e: ProgErr)

final case class CleanProgram(
    decls: List[CleanDecl], 
    stmts: List[CleanStmt], 
    expr: CleanExpr
)


// Declaration ::= (def Variable Expression)
enum DeclWE:
    case Def(lhs: VarWE, rhs: ExprWE)
    case Err(e: DeclErr)

final case class CleanDecl(
    lhs: CleanVar,
    rhs: CleanExpr
)


//   Statement  ::= (Variable = Expression)
//                | (if0 Expression Block Block)
//                | (while0 Expression Block)
enum StmtWE:
    case Assign(lhs: VarWE, rhs: ExprWE)
    case Ifelse(guard: ExprWE, tbranch: BlockWE, ebranch: BlockWE)
    case While(guard: ExprWE, body: BlockWE)
    case Err(e: StmtErr)

enum CleanStmt:
    case Assign(lhs: CleanVar, rhs: CleanExpr)
    case Ifelse(guard: CleanExpr, tbranch: CleanBlock, ebranch: CleanBlock)
    case While(guard: CleanExpr, body: CleanBlock)


//   Block      ::= Statement
//                | (block Declaration^* Statement^+)
enum BlockWE:
    case One(stmt: StmtWE)
    case Many(decls: List[DeclWE], stmts: List[StmtWE])
    case Err(e: BlockErr)

enum CleanBlock:
    case One(stmt: CleanStmt)
    case Many(decls: List[CleanDecl], stmts: List[CleanStmt])


//   Expression ::= GoodNumber
//                | Variable
//                | (Variable + Variable)
//                | (Variable / Variable)
//                | (Variable == Variable)
enum ExprWE:
    case Num(n: Double)
    case Var(x: String)
    case BinOpExpr(lhs: VarWE, op: BinOp, rhs: VarWE)
    case Err(e: ExprErr)
    case VarErrNode(e : VarErr)

type VarWE = ExprWE.Var | ExprWE.VarErrNode

enum CleanExpr:
    case Num(n: NumVal)
    case Var(x: String)
    case BinOpExpr(lhs: CleanVar, op: BinOp, rhs: CleanVar)

type CleanVar = CleanExpr.Var

enum BinOp:
    case Add
    case Div
    case Equals

// Inexact numbers that are understood to be Values
type NumVal = Double