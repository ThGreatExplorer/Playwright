package ast

/******************************************************************************
  Type AST 
 *****************************************************************************/

// Type       ::= Number | Shape 
enum Type[Node[_]]:
    case Number()
    case Shape(
        fieldTypes:  List[Node[FieldType[Node]]],
        methodTypes: List[Node[MethodType[Node]]]
    )

type TypeWE    = WE[Type[WE]]
type CleanType = Clean[Type[Clean]]

type ShapeTypeWE = WE[Type.Shape[WE]]
type CleanShapeType = Clean[Type.Shape[Clean]]

// FieldType  ::= (FieldName Type) 
final case class FieldType[Node[_]](
    fname:     Node[Name],
    fieldType: Node[Type[Node]]
)

type FieldTypeWE = WE[FieldType[WE]]
type CleanFieldType = Clean[FieldType[Clean]]

// MethodType ::= (MethodName (Type^*) Type) 
final case class MethodType[Node[_]](
    mname:      Node[Name],
    paramTypes: List[Node[Type[Node]]],
    returnType: Node[Type[Node]]
)

type MethodTypeWE = WE[MethodType[WE]]
type CleanMethodType = Clean[MethodType[Clean]]
