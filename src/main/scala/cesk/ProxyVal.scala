package cesk

import ast._
import util.getFTypeNames

final case class ProxyVal(
	obj: ObjectVal,
	typ: CleanShapeType
):

    def conformToFieldType(field: String, v : CESKValue, classDefs: ClassDefs): Either[RuntimeError, CESKValue] =
        for 
            ftype        <- getFieldType(field)
            conformedVal <- ProxyVal.conformToType(v, ftype, classDefs)
        yield
            conformedVal

    def lookupConformingField(field: String, classDefs: ClassDefs): Either[RuntimeError, CESKValue] =
        for
            storedVal <- this.obj.lookupField(field)
            conformed <- conformToFieldType(field, storedVal, classDefs)
        yield
            conformed

    def updateField(field: String, v: CESKValue): Unit =
        this.obj.updateField(field, v)

    def getFieldType(field: String) : Either[RuntimeError, CleanType] = 
        val ftypeOpt = this.typ
                           .fieldTypes
                           .find{case FieldType(fname, _) => fname == field}

        ftypeOpt match
            case Some(FieldType(_, fieldType)) => Right(fieldType) 
            case None => Left(RuntimeError.FieldNotFoundInProxy)
        
    override def equals(that: Any): Boolean = that match
        case ProxyVal(thatObj, thatTyp) => 
            this.obj.equals(thatObj)
            &&
            this.typ == thatTyp
        case _ => false

object ProxyVal:
    def conformToType(v : CESKValue, expTyp : CleanType, classDefs: ClassDefs) : Either[RuntimeError, CESKValue] =
        (v, expTyp) match
            case (num : NumVal, Type.Number) => 
                Right(num)

            case (ProxyVal(obj, sh), expShape : CleanShapeType) =>
                if sh == expShape then
                    Right(v)
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







                
            
                
                
                
         