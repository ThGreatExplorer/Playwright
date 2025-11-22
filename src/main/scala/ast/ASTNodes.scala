/******************************************************************************
  This file defines data represetnation for Class, and Module (with optional 
  type annotations) ASTs. The two AST definitons rely on the shared set of 
  sub-tree constructors, so we define them in the same file. 

  We use parametrized type definitons to help us create two versions for each
  AST node type:
    - NameWE: represents an AST node that can potentically include error nodes
      in its chidren nodes or be an error node itself.
      (WE stands for "with error")
    - CleanName: represents an AST node that does not have error nodes in
      any of its children and is itself a valid node.  

 *****************************************************************************/

package ast
import static.ModuleData

type Clean[+A] = A
enum WE[+A]:
    case Node(n : A)
    case Err(e : ParseErrNodes | ValidityErrNodes | TypeErrorNodes)

/******************************************************************************
  Mixed AST (with Typed and Untyped Modules)
 *****************************************************************************/

//  MixedSystem ::= (MixedModule^* MixedImport^* Declaration^* Statement^* Expression)

// Used in early stages of the pipeline, while basic invariants are still not established
final case class RawSystem[Node[_]](
    modules: List[Node[Module[Node]]],
    imports: List[Node[Import[Node]]],
    progb:   Node[ProgBlock[Node]]
)
type CleanRawSystem = Clean[RawSystem[Clean]]
type RawSystemWE    = WE[RawSystem[WE]]

// Used by more complex passes of the compiler
final case class System[Node[_]](
    modules: List[Node[Module[Node]]],
    imports: List[Node[Import[Node]]],
    progb:   Node[ProgBlock[Node]],
    // Map for quick global look ups of Module-related Data, helps to avoid rediscovering
    // module-class name-type association as well as scoped information 
    moddata: ModuleData
)
type CleanSystem = Clean[System[Clean]]
type SystemWE    = WE[System[WE]]

//  Module      ::= (module  ModuleName Import^* Class)
// Note that typed modules (defined as modules with class with shape) can have mixed imports
// but untyped modules (modules with class without shape) can only have untyped imports
final case class Module[Node[_]](
    mname: Node[Name],
    imports: List[Node[Import[Node]]], 
    clas: Node[Class[Node]]
)

type CleanModule = Clean[Module[Clean]]
type ModuleWE    = WE[Module[WE]]

//  MixedImport ::= (import ModuleName)
//          | (timport ModuleName Shape)
enum Import[Node[_]]:
    case Untyped(mname: Node[Name]) 
    case Typed  (mname: Node[Name], shape: Node[Type.Shape[Node]])
type CleanImport = Clean[Import[Clean]]
type ImportWE    = WE[Import[WE]]

type CleanUntypedImport = Clean[Import.Untyped[Clean]]
type UntypedImportWE    = WE[Import.Untyped[WE]]

// Import utils
extension (imported: Clean[Import[Clean]])
    def importedModName: String = imported match
        case Import.Typed(mname, _) => mname
        case Import.Untyped(mname) => mname

/******************************************************************************
  Class AST
 *****************************************************************************/

// Program     ::= (Class^* Declaration^* Statement^* Expression)
final case class Program[Node[_]](
    clss:  List[Node[Class[Node]]],
    progb: Node[ProgBlock[Node]]
)

type CleanProgram = Clean[Program[Clean]]
type ProgramWE    = WE[Program[WE]]

// Class       ::= (class ClassName (FieldName^*) Method^* Shape | None)
final case class Class[Node[_]](
    cname:   Node[Name], 
    fields:  List[Node[Name]], 
    methods: List[Node[Method[Node]]],
    shape: Option[Node[Type.Shape[Node]]]
)

type CleanClass = Clean[Class[Clean]]
type ClassWE    = WE[Class[WE]]

// Method      ::= (method MethodName (Parameter^*)
//                      Declaration^* Statement^* Expression)
final case class Method[Node[_]](
    mname:  Node[Name], 
    params: List[Node[Name]], 
    progb:  Node[ProgBlock[Node]]
)

type CleanMethod = Clean[Method[Clean]]
type MethodWE    = WE[Method[WE]]

/******************************************************************************
  Core AST 
 *****************************************************************************/

// Core computation block ::= Declaration^* Statement^* Expression 
// Not in the grammar, but makes sense to abstract (occurs in System, Program, Method)
final case class ProgBlock[Node[_]](
    decls: List[Node[Decl[Node]]], 
    stmts: List[Node[Stmt[Node]]], 
    expr:  Node[Expr[Node]]
)

type CleanProgBlock = Clean[ProgBlock[Clean]]
type ProgBlockWE    = WE[ProgBlock[WE]]

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
    case Ifelse(guard: Node[Expr[Node]], tbranch: Node[StmtBlock[Node]], ebranch: Node[StmtBlock[Node]])
    case While(guard: Node[Expr[Node]], body: Node[StmtBlock[Node]])
    case FieldAssign(instance: Node[VarRef], field: Node[Name], rhs: Node[Expr[Node]])

type CleanStmt = Clean[Stmt[Clean]]
type StmtWE    = WE[Stmt[WE]]

//   Block      ::= Statement
//                | (block Declaration^* Statement^+)
enum StmtBlock[Node[_]]:
    case One(stmt: Node[Stmt[Node]])
    case Many(
        decls: List[Node[Decl[Node]]], 
        stmts: List[Node[Stmt[Node]]]
    )

type CleanStmtBlock = Clean[StmtBlock[Clean]]
type StmtBlockWE    = WE[StmtBlock[WE]]

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

type VarRef = String
type CleanVarRef = Clean[VarRef]
type VarRefWE    = WE[VarRef]

type Name = String
type CleanName = Clean[Name]
type NameWE    = WE[Name]
    
// Inexact numbers that are understood to be Values
type NumVal = Double