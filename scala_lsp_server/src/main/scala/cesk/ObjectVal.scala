package cesk

import util.{=~=, isZero}
import ast.NumVal
import scala.collection.mutable.Map as MutableMap
import scala.collection.immutable.Set

final case class ObjectVal(
	className: String,
	fieldVals: MutableMap[String, CESKValue]
):

    def lookupField(field: String): Either[RuntimeError, CESKValue] =
        fieldVals.get(field).toRight(RuntimeError.FieldNotFound)

    def updateField(field: String, v: CESKValue): Unit =
        this.fieldVals(field) = v

    def checkMethod(classDefs: ClassDefs, methodName: String, argVals: List[CESKValue]): Either[RuntimeError, MethodDef] =
        classDefs.getMethodDef(className, methodName) match
            case Left(rtErr) => 
                Left[RuntimeError, MethodDef](rtErr)
            case Right(MethodDef(params, progFrame)) =>
                if params.lengthIs != argVals.length then
                    Left[RuntimeError, MethodDef](RuntimeError.MethodCallWrongArgCount)
                else 
                    Right[RuntimeError, MethodDef](MethodDef(params, progFrame))

    override def equals(that: Any): Boolean = that match
        case that: ObjectVal => ObjectVal.structurallyEqual(this, that)
        case _ => false


object ObjectVal:

    private def structurallyEqual(o1 : ObjectVal, o2: ObjectVal): Boolean =

        // To prevent recrusive comparison with equals() within Set[ObjectVal], 
        // we keep track of object references instead
        type ObjRefPair = (AnyRef, AnyRef)

        def namesAndMapsEqual(o1 : ObjectVal, o2: ObjectVal, updSeen: Set[ObjRefPair]) : Boolean = 
            (o1, o2) match
                case (ObjectVal(name1, map1), ObjectVal(name2, map2)) if (name1 == name2) =>
                    map1.forall { case (key, val1) =>  
                        // We already checked that objects have the same class name, 
                        // so we know that every key lookup will succeed
                        val val2 = map2(key)

                        (val1, val2) match
                            case (num1 : NumVal, num2 : NumVal) => 
                                num1 =~= num2
                            case (obj1 : ObjectVal, obj2 : ObjectVal) => 
                                structEqualHelp(obj1, obj2, updSeen)
                            case _ => false
                    }

                case _ => false

        def structEqualHelp(o1 : ObjectVal, o2: ObjectVal, seen: Set[ObjRefPair]) : Boolean = 
            val curPair = (o1, o2)

            (o1 eq o2)                 // Referential equality 
            || seen.contains(curPair)  // Seen this pair before
            || namesAndMapsEqual(o1, o2, seen + curPair)

        structEqualHelp(o1, o2, Set[ObjRefPair]())
