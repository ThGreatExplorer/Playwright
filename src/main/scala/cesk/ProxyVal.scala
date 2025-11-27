package cesk

import ast._
import util.{getFTypeNames, traverseEither}
import cesk.ProxyVal.conformToType

final case class ProxyVal(
	obj: ObjectVal,
	typ: CleanShapeType
):

    // Field Lookup & Assignment

    def getFieldType(field: String) : Either[RuntimeError, CleanFieldType] = 
        val ftypeOpt = this.typ
                           .fieldTypes
                           .find{case FieldType(fname, _) => fname == field}

        ftypeOpt.toRight(RuntimeError.FieldNotFoundInProxy)

    def lookupConformingField(field: String, classDefs: ClassDefs): Either[RuntimeError, CESKValue] =
        for
            storedVal    <- this.obj.lookupField(field)
            ftype        <- getFieldType(field)
            conformedVal <- ProxyVal.conformToType(storedVal, ftype.fieldType, classDefs)
        yield
            conformedVal

    def conformToFieldType(field: String, v : CESKValue, classDefs: ClassDefs): Either[RuntimeError, CESKValue] =
        for 
            ftype        <- getFieldType(field)
            conformedVal <- ProxyVal.conformToType(v, ftype.fieldType, classDefs)
        yield
            conformedVal

    def updateField(field: String, v: CESKValue): Unit =
        this.obj.updateField(field, v)

    // Method Call

    def getMethodType(mname: String): Either[RuntimeError, CleanMethodType] =
        val mtypeOpt = this.typ
                           .methodTypes
                           .find{case MethodType(name, paramTypes, returnType) => mname == name}
        
        mtypeOpt.toRight(RuntimeError.MethodNotFoundInProxy)

    def conformArgsToMethodDomTypes(
        pTypes: List[CleanType], aVals: List[CESKValue], classDefs: ClassDefs
    ): Either[RuntimeError, List[CESKValue]] = 
        if pTypes.lengthIs != aVals.length then
            Left(RuntimeError.MethodCallDoesntMatchProxyMethodType)
        else
            val conformedParams: Either[RuntimeError, List[CESKValue]] = 
            traverseEither(pTypes.zip(aVals)) { case (pType, aVal) =>
                conformToType(aVal, pType, classDefs)
            }
            conformedParams match
                case Left(_) => Left(RuntimeError.MethodCallDoesntMatchProxyMethodType)
                case Right(conformedPVals) => Right(conformedPVals)
                
    def checkConformingMethod(
        mname: String, aVals: List[CESKValue], classDefs: ClassDefs
    ): Either[RuntimeError, (MethodDef, List[CESKValue], CleanType)] =
        for
            mtype              <- getMethodType(mname)
            conformedAVals     <- conformArgsToMethodDomTypes(mtype.paramTypes, aVals, classDefs)
            checkedMethod      <- this.obj.checkMethod(classDefs, mname, conformedAVals)
        yield
            (checkedMethod, conformedAVals, mtype.returnType)
        
    // Structural Equality
    override def equals(that: Any): Boolean = that match
        case ProxyVal(thatObj, thatTyp) => 
            this.obj.equals(thatObj)
            &&
            this.typ == thatTyp
        case _ => false

object ProxyVal:
    def conformToType(v : CESKValue, expTyp : CleanType, classDefs: ClassDefs) : Either[RuntimeError, CESKValue] =
        (v, expTyp) match
            case (num : NumVal, Type.Number()) => 
                Right(num)

            case (prx @ ProxyVal(obj, sh), expShape : CleanShapeType) =>
                if sh == expShape then
                    Right(prx)
                else 
                    Left(RuntimeError.ProxyValDoesntConformToProxyShape)

            case (v @ ObjectVal(cname, fieldValsMap), expTyp @ Type.Shape(ftypes, mtypes)) =>
                if ftypes.getFTypeNames.toSet != fieldValsMap.keySet then
                    Left(RuntimeError.FieldNamesDontConformToProxyShape) 

                else if !ftypes.forall{
                    case FieldType(fname, ftype) => 
                        conformToType(fieldValsMap(fname), ftype, classDefs).isRight
                } then
                    Left(RuntimeError.FieldValsDontConformToProxyShape)

                else 
                    classDefs.methodsOfClassConformToTypes(cname, mtypes)
                             .map(_ => ProxyVal(v, expTyp))

            case _ =>
                Left(RuntimeError.ValDoesntConformToExpType)
