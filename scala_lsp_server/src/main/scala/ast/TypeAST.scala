package ast

import sexprs.Range

// Note: Inferred types carry the Range of the expression they describe

/******************************************************************************
  Type AST 
 *****************************************************************************/

// Type       ::= Number | Shape 
enum Type[Node[_]]:
    case Number(range: Range)
    case Shape(
        fieldTypes:  List[Node[FieldType[Node]]],
        methodTypes: List[Node[MethodType[Node]]],
        range:       Range
    )

type TypeWE    = WE[Type[WE]]
type CleanType = Clean[Type[Clean]]

type ShapeTypeWE = WE[Type.Shape[WE]]
type CleanShapeType = Clean[Type.Shape[Clean]]

// FieldType  ::= (FieldName Type) 
final case class FieldType[Node[_]](
    fname:     Node[Name],
    fieldType: Node[Type[Node]],
    range:     Range
)

type FieldTypeWE = WE[FieldType[WE]]
type CleanFieldType = Clean[FieldType[Clean]]

// MethodType ::= (MethodName (Type^*) Type) 
final case class MethodType[Node[_]](
    mname:      Node[Name],
    paramTypes: List[Node[Type[Node]]],
    returnType: Node[Type[Node]],
    range:      Range
)

type MethodTypeWE = WE[MethodType[WE]]
type CleanMethodType = Clean[MethodType[Clean]]
