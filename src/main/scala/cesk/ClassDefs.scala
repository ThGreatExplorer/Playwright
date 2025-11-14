package cesk

import ast._
import util.UnreachablePatternMatch
import scala.collection.mutable.Map as MutableMap
import util.{getMNames, getCNames}

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
    methods: Map[Name, MethodDef]
)

object ClassDef:
    def apply(clas : CleanClass) : ClassDef = clas match
        case Class(cname, fields, methods) => 
            val methodDefs = methods.map(MethodDef(_))
            val methodMap = methods.getMNames.zip(methodDefs).toMap

            ClassDef(fields,methodMap)
    

trait ClassDefs:
    def getClassDef(className: String): ClassDef
    def getMethodDef(className: String, methodName: String): Either[RuntimeError, MethodDef]
    def getInstanceOfClass(className : String, argVals : List[CESKValue]) : Either[RuntimeError, ObjectVal]
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
                case ClassDef(fields, methods) => 
                    methods.get(methodName).toRight(RuntimeError.MethodNotFound)
        
        def getInstanceOfClass(className : String, argVals : List[CESKValue]) : Either[RuntimeError, ObjectVal] =
            getClassDef(className) match
                case ClassDef(fields, methods) =>
                    if fields.lengthIs != argVals.length  then
                        Left[RuntimeError, ObjectVal](RuntimeError.NewInstWrongFieldCount)
                    else
                        val fieldMap = MutableMap.from(fields.zip(argVals)) 
                        Right[RuntimeError, ObjectVal](ObjectVal(className, fieldMap)) 
