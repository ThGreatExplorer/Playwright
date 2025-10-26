package cesk

import ast._
// import scala.annotation.meta.field
import util.{UnreachablePatternMatch, =~=, isZero}
import ast.Expr.Num
import scala.collection.mutable.Map as MutableMap
import scala.reflect.Selectable.reflectiveSelectable
import java.sql.Ref

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
					Right(ObjectVal(className, MutableMap(fieldMap.toSeq*)))


case class ObjectVal(
	className: String,
	fieldVals: MutableMap[String, CESKValue]
):

	private class RefSet[A <: AnyRef](underlying: List[A]) extends Seq[A] {

		def length: Int = underlying.length
		def apply(idx: Int): A = underlying(idx)
		def iterator: Iterator[A] = underlying.iterator

		def addElem(value: A): RefSet[A] =
			if underlying.forall(obj => obj ne value) then
				RefSet(value :: underlying)
			else
				this
  
		def containsByRef(value: A): Boolean =
			underlying.exists(obj => obj eq value)
	}

	def lookupField(field: String): Either[RuntimeError, CESKValue] =
		fieldVals.get(field).toRight(RuntimeError.FieldNotFound)

	def updateField(field: String, v: CESKValue): Unit =
		this.fieldVals(field) = v

	def checkMethod(classDefs: ClassDefs, methodName: String, argVals: List[CESKValue]): Either[RuntimeError, MethodCallConstructorData] =
		classDefs.getMethod(className, methodName) match
			case Left(rtErr) => Left(rtErr)
			case Right(MethodCallConstructorData(params, progFrame)) if params.lengthIs != argVals.length =>
				Left(RuntimeError.MethodCallWrongArgCount)
			case Right(m) => Right(m)

	private def structuralEquality(that: ObjectVal): Boolean =
		def addElemByReferenceToSet(objValsSeen: List[ObjectVal], value: ObjectVal): List[ObjectVal] =
			if objValsSeen.forall(obj => obj ne value) then
				value :: objValsSeen
			else
				objValsSeen

		def checkElemInSet(objValsSeen: List[ObjectVal], value: ObjectVal): Boolean =
			objValsSeen.exists(obj => obj eq value)

		def mapEquals(curr: MutableMap[String, CESKValue], target: MutableMap[String, CESKValue], objValsSeen: RefSet[ObjectVal], level: Int) : Boolean =
			curr.size == target.size && curr.forall { case (key, currVal) =>
				target.get(key) match
					case None => false
					case Some(targetVal) => 
						(currVal, targetVal) match
							case (currNum : NumVal, targetNum : NumVal) => currNum =~= targetNum
							case (currObj : ObjectVal, targetObj : ObjectVal) => 
								val updatedFieldValsSeen = objValsSeen.addElem(currObj)
								if objValsSeen.containsByRef(currObj) then 
									true
								else 
									val updatedUpdatedFieldValsSeen = updatedFieldValsSeen.addElem(targetObj)
									equalsHelper(currObj, targetObj, updatedFieldValsSeen, level + 1)
							case (_, _) => false
			}

		def equalsHelper(currObj: ObjectVal, targetObj: ObjectVal, objValsSeen: RefSet[ObjectVal], level: Int): Boolean =
			currObj match
				case ObjectVal(currName, currFieldVals) =>
					currName.equals(targetObj.className) && mapEquals(currFieldVals, targetObj.fieldVals, objValsSeen, level)
		
		equalsHelper(this, that, RefSet(List(this)), 0)
	
	override def equals(that: Any): Boolean = 	
		that match
				case that: ObjectVal =>
					structuralEquality(that)
				case _ => false

	override def hashCode(): Int = 
		(className, fieldVals).hashCode()