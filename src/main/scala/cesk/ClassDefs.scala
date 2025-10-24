package cesk

import ast._
// import scala.annotation.meta.field
import util.UnreachablePatternMatch
import ast.Expr.Num

final case class MethodCallConstructorData(
	params: List[CleanName],
	progFrame: ProgFrame
)

final case class ClassDef(
	fields: List[String],
	methods: Map[String, MethodCallConstructorData]
)

trait ClassDefs:
	def getClass(className: String): ClassDef
	def getMethod(className: String, methodName: String): Either[RuntimeError, MethodCallConstructorData]
	def getInstanceOfClass(className : String, argVals : List[CESKValue]) : Either[RuntimeError, ObjectVal]
	override def toString(): String

object ClassDefs:

	def apply(classes: List[CleanClass]): ClassDefs =
		val mapClassDef = classes.map(clss => clss match
			case Class[Clean](Name(className), fields, methods) => 
				val classDef = ClassDef(
					fields.map(field => field.x),
					methods.map(method => method.mname.x -> 
						MethodCallConstructorData(method.params, ProgFrame(method.decls, method.stmts, method.expr))
					).toMap
				)
				(className, classDef)
		).toMap
		new MapClassDefs(mapClassDef)

	private class MapClassDefs(underlying: Map[String, ClassDef]) extends ClassDefs:
		override def toString(): String = underlying.toString()

		def getClass(className: String): ClassDef =
			underlying.get(className) match
				case Some(cDef) => cDef
				case None =>   
					// Technically unnecessary throw but it will help us catch env errors early
					throw new UnreachablePatternMatch(
						"Should never happen: className " + className + " not found in ClassDefs: " + underlying.toString
					)

		def getMethod(className: String, methodName: String): Either[RuntimeError, MethodCallConstructorData] =
			getClass(className) match
				case ClassDef(fields, methods) => 
					methods.get(methodName).toRight(RuntimeError.MethodNotFound)
		
		def getInstanceOfClass(className : String, argVals : List[CESKValue]) : Either[RuntimeError, ObjectVal] =
			getClass(className) match
				case ClassDef(fields, methods) if fields.lengthIs != argVals.length =>
					Left(RuntimeError.NewInstWrongFieldCount) 
				case ClassDef(fields, methods) =>
					val fieldMap = fields.zip(argVals).toMap
					Right(ObjectVal(className, fieldMap))


final case class ObjectVal(
	className: String,
	fieldVals: Map[String, CESKValue]
):
	def lookupField(field: String): Either[RuntimeError, CESKValue] =
		fieldVals.get(field).toRight(RuntimeError.FieldNotFound)

	def updateField(field: String, v: CESKValue): ObjectVal =
		new ObjectVal(className, fieldVals.updated(field, v))

	def checkMethod(classDefs: ClassDefs, methodName: String, argVals: List[CESKValue]): Either[RuntimeError, MethodCallConstructorData] =
		classDefs.getMethod(className, methodName) match
			case Left(rtErr) => Left(rtErr)
			case Right(MethodCallConstructorData(params, progFrame)) if params.lengthIs != argVals.length =>
				Left(RuntimeError.MethodCallWrongArgCount)
			case Right(m) => Right(m)
	
	override def equals(that: Any): Boolean = 
		that match
			case ObjectVal(thatName, thatFieldVals) =>
				thatName.equals(className) && this.fieldVals.equals(thatFieldVals)
			case _ => false