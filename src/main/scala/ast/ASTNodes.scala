/******************************************************************************
  This file defines data represetnation for Type, Class, and Module ASTs. The three AST
  definitons rely on the shared set of sub-tree constructors, so we define them
  in the same file. 

  Module is a modification of the class AST and Types is modification on top of modules,
  adding type information through introducing typed modules.

  We use parametrized type definitons to help us create two versions for each
  AST node type:
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
    case Err(e : ParseErrNodes | ValidityErrNodes | TypeErrorNodes)

/******************************************************************************
  Type AST 
 *****************************************************************************/
/*  Type       ::= Number | Shape */
type ASType[Node[_]] = "Number" | Node[Shape[Node]]
type ASTypeWE = WE[ASType[WE]]
type CleanASType = Clean[ASType[Clean]]

/* TypedSystem ::= (TypedModule^*
                   Import^*
                   Declaration^*
                   Statement^*
                   Expression) */
final case class TypedSystem[Node[_]](
    modules: List[Node[TypedModule[Node]]],
    imports: List[Node[ImportedMod]],
    progb:   Node[ProgBlock[Node]]
)

type TypedSystemWE = WE[TypedSystem[WE]]
type CleanTypedSystem = Clean[TypedSystem[Clean]]

/* TypedModule ::= (tmodule ModuleName Import^* Class Shape) */
final case class TypedModule[Node[_]](
    mname: Node[Name],
    imports: List[Node[ImportedMod]],
    clas:    Node[Class[Node]],
    shape: Node[Shape[Node]]
)

type TypedModuleWE = WE[TypedModule[WE]]
type CleanTypedModule = Clean[TypedModule[Clean]]

/*Shape      ::= ((FieldType^*) (MethodType^*)) */
final case class Shape[Node[_]](
    fieldTypes: List[Node[FieldType[Node]]],
    methodType: List[Node[MethodType[Node]]]
)

type ShapeWE = WE[Shape[WE]]
type CleanShape = Clean[Shape[Clean]]

/*MethodType ::= (MethodName (Type^*) Type) */
final case class MethodType[Node[_]](
    mname: Node[Name],
    paramTypes: List[Node[ASType[Node]]],
    returnType: Node[ASType[Node]]
)

type MethodTypeWE = WE[MethodType[WE]]
type CleanMethodType = Clean[MethodType[Clean]]

/* FieldType  ::= (FieldName Type) */
final case class FieldType[Node[_]](
    fname: Node[Name],
    fieldType: Node[ASType[Node]]
)

type FieldTypeWE = WE[FieldType[WE]]
type CleanFieldType = Clean[FieldType[Clean]]

/******************************************************************************
  Module AST
 *****************************************************************************/

//  System ::= (Module^* Import^* Declaration^* Statement^* Expression)
final case class System[Node[_]](
    modules: List[Node[Module[Node]]],
    imports: List[Node[ImportedMod]],
    progb:   Node[ProgBlock[Node]]
)

type CleanSystem = Clean[System[Clean]]
type SystemWE    = WE[System[WE]]

//  Module ::= (module ModuleName Import^* Class)
final case class Module[Node[_]](
    mname: Node[Name],
    imports: List[Node[ImportedMod]],
    clas:    Node[Class[Node]]
)

type CleanModule = Clean[Module[Clean]]
type ModuleWE    = WE[Module[WE]]

extension (clss : List[Module[Clean]])
    def getMDNames : List[String] = clss.map{ case Module(mname, _, _) => mname }

//  Import ::= (import ModuleName)
type ImportedMod = String
type CleanImportedMod = Clean[ImportedMod]
type ImportedModWE    = WE[ImportedMod]

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

// Class       ::= (class ClassName (FieldName^*) Method^*)
final case class Class[Node[_]](
    cname:   Node[Name], 
    fields:  List[Node[Name]], 
    methods: List[Node[Method[Node]]]
)

type CleanClass = Clean[Class[Clean]]
type ClassWE    = WE[Class[WE]]

extension (clss : List[Class[Clean]])
    def getCNames : List[String] = clss.map{ case Class(cname, _, _) => cname }

// Method      ::= (method MethodName (Parameter^*)
//                      Declaration^* Statement^* Expression)
final case class Method[Node[_]](
    mname:  Node[Name], 
    params: List[Node[Name]], 
    progb:  Node[ProgBlock[Node]]
)

type CleanMethod = Clean[Method[Clean]]
type MethodWE    = WE[Method[WE]]

extension (methods : List[Method[Clean]])
    def getMNames : List[String] = methods.map{ case Method(mname, _, _) => mname }

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