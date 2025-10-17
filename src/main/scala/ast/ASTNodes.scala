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

type Clean[A] = A
enum WE[A]:
    case Node(n : A)
    case Err(e : ParseErrNodes | ValidityErrNodes)

// Program     ::= (Class^* Declaration^* Statement^* Expression)
final case class Program[Node[_]](
    clss:  List[Node[Class[Node]]],
    decls: List[Node[Decl[Node]]],
    stmts: List[Node[Stmt[Node]]],
    expr:  Node[Expr[Node]]
)

type CleanProgram = Clean[Program[Clean]]
type ProgramWE    = WE[Program[WE]]

// Class       ::= (class ClassName (FieldName^*) Method^*)
final case class Class[Node[_]](
    cname:   Node[Name], 
    fields:  List[Node[Name]], 
    methods: List[Node[Method[Node]]]
)

type CleanClass = Clean[Class[Clean]]
type ClassWE    = WE[Class[WE]]

// Method      ::= (method MethodName (Parameter^*)
//                      Declaration^* Statement^* Expression)
final case class Method[Node[_]](
    mname:  Node[Name], 
    params: List[Node[Name]], 
    decls:  List[Node[Decl[Node]]], 
    stmts:  List[Node[Stmt[Node]]], 
    expr:   Node[Expr[Node]]
)

type CleanMethod = Clean[Method[Clean]]
type MethodWE    = WE[Method[WE]]

// Declaration ::= (def Variable Expression)
final case class Decl[Node[_]](
    varDecl: Node[Name],
    rhs: Node[Expr[Node]]
)

type CleanDecl = Clean[Decl[Clean]]
type DeclWE    = WE[Decl[WE]]

//   Statement  ::= (Variable = Expression)
//                | (if0 Expression Block Block)
//                | (while0 Expression Block)
//                | (Variable --> FieldName = Expression)
enum Stmt[Node[_]]:
    case Assign(lhs: Node[VarRef], rhs: Node[Expr[Node]])
    case Ifelse(guard: Node[Expr[Node]], tbranch: Node[Block[Node]], ebranch: Node[Block[Node]])
    case While(guard: Node[Expr[Node]], body: Node[Block[Node]])
    case FieldAssign(instance: Node[VarRef], field: Node[Name], rhs: Node[Expr[Node]])

type CleanStmt = Clean[Stmt[Clean]]
type StmtWE    = WE[Stmt[WE]]


//   Block      ::= Statement
//                | (block Declaration^* Statement^+)
enum Block[Node[_]]:
    case One(stmt: Node[Stmt[Node]])
    case Many(decls: List[Node[Decl[Node]]], stmts: List[Node[Stmt[Node]]])

type CleanBlock = Clean[Block[Clean]]
type BlockWE    = WE[Block[WE]]

//   Expression ::= GoodNumber
//                | Variable
//                | (Variable + Variable)
//                | (Variable / Variable)
//                | (Variable == Variable)
//                | (new ClassName (Variable^*))
//                | (Variable --> FieldName)
//                | (Variable --> MethodName (Variable^*))
//                | (Variable isa ClassName)
enum Expr[Node[_]]:
    case Num(n: NumVal)
    case Var(x: Node[VarRef])
    case BinOpExpr(lhs: Node[VarRef], op: BinOp, rhs: Node[VarRef])
    case NewInstance(cname: Node[Name], args: List[Node[VarRef]])
    case GetField(instance: Node[VarRef], field: Node[Name])
    case CallMethod(instance: Node[VarRef], method: Node[Name], args: List[Node[VarRef]])
    case IsInstanceOf(instance: Node[VarRef], cname: Node[Name])

enum BinOp:
    case Add, Div, Equals

type CleanExpr = Expr[Clean]
type ExprWE    = WE[Expr[WE]]

final case class VarRef(x: String)
type CleanVarRef = Clean[VarRef]
type VarRefWE    = WE[VarRef]

// TODO: add implicit coersion to string? 
final case class Name(x: String)
type CleanName = Clean[Name]
type NameWE    = WE[Name]
    
// Inexact numbers that are understood to be Values
type NumVal = Double