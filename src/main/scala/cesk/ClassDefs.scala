package cesk

import ast._
import util.UnreachablePatternMatch
import scala.collection.mutable.Map as MutableMap
import util.{getMNames, getCNames, getMTypeNames}

final case class MethodDef(
    params: List[Name],
    progFrame: ProgFrame
)

object MethodDef:
    def apply(method : CleanMethod) : MethodDef = method match
        case Method(_, params, ProgBlock(decls, stmts, expr)) => 
            MethodDef(params, ProgFrame(decls, stmts, expr))
    

final case class ClassDef(
    fields: List[Name],
    methods: Map[Name, MethodDef],
    shape: Option[CleanShapeType] // can't guarantee that all classes have shapes, for example Class A in untyped module A 
    // imported into untyped module B imported into top level prog wouldn't have a shape
)

object ClassDef:
    def apply(clas : CleanClass) : ClassDef = clas match
        case Class(cname, fields, methods, shape) => 
            val methodDefs = methods.map(MethodDef(_))
            val methodMap = methods.getMNames.zip(methodDefs).toMap

            ClassDef(fields,methodMap, shape)
    

trait ClassDefs:
    def getClassDef(className: String): ClassDef
    def getMethodDef(className: String, methodName: String): Either[RuntimeError, MethodDef]
    def getInstanceOfClass(className : String, argVals : List[CESKValue]) : Either[RuntimeError, CESKValue]
    def methodsOfClassConformToTypes(className : String, mtypes : List[CleanMethodType]) : Either[RuntimeError, Unit]
    override def toString(): String

object ClassDefs:

    def apply(classes: List[CleanClass]): ClassDefs =
        val classDefList = classes.map(ClassDef(_))
        val classDefMap = classes.getCNames.zip(classDefList).toMap
        new MapClassDefs(classDefMap)

    private class MapClassDefs(underlying: Map[String, ClassDef]) extends ClassDefs:
        override def toString(): String = underlying.toString()

        def getClassDef(className: String): ClassDef =
            underlying.get(className) match
                case Some(cDef) => cDef
                case None =>   
                    // Technically unnecessary throw but it will help us catch env errors early
                    throw new UnreachablePatternMatch(
                        "Should never happen: className " + className + " not found in ClassDefs: " + underlying.toString
                    )

        def getMethodDef(className: String, methodName: String): Either[RuntimeError, MethodDef] =
            getClassDef(className) match
                case ClassDef(fields, methods, _) => 
                    methods.get(methodName).toRight(RuntimeError.MethodNotFound)
        
        def getInstanceOfClass(className : String, argVals : List[CESKValue]) : Either[RuntimeError, CESKValue] =
            getClassDef(className) match
                case ClassDef(fields, _, _) if fields.lengthIs != argVals.length =>
                    Left(RuntimeError.NewInstWrongFieldCount)

                case ClassDef(fields, methods, None) => 
                    val fieldMap = MutableMap.from(fields.zip(argVals)) 
                    val obj      = ObjectVal(className, fieldMap)
                    Right(ObjectVal(className, fieldMap)) 

                case ClassDef(fields, methods, Some(shape)) =>
                    val fieldMap = MutableMap.from(fields.zip(argVals)) 
                    val obj      = ObjectVal(className, fieldMap)
                    ProxyVal.conformToType(obj, shape, this)

        def methodsOfClassConformToTypes(className : String, mtypes : List[CleanMethodType]) : Either[RuntimeError, Unit] =
            getClassDef(className) match
                case ClassDef(_, methods, _) if mtypes.getMTypeNames.toSet != methods.keySet =>
                    Left(RuntimeError.MethodNamesDontConformToProxyShape)

                case ClassDef(_, methods, _) =>
                    val conforms = 
                        mtypes.forall{
                            case MethodType(mname, paramTypes, _) => 
                                methods(mname).params.lengthIs == paramTypes.length
                        }
                    
                    if conforms then 
                        Right(())
                    else 
                        Left(RuntimeError.MethodParamsDontConformToProxyShape)
