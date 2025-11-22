package cesk

import ast._
import util.getFTypeNames

final case class ProxyVal(
	obj: ObjectVal,
	typ: CleanShapeType
):

    def lookupField(field: String): Either[RuntimeError, CESKValue] =
        this.obj.lookupField(field)

    def updateField(field: String, v: CESKValue): Unit =
        this.obj.updateField(field, v)

    def getFieldType(field: String) : Either[RuntimeError, CleanType] = 
        val ftypeOpt = this.typ
                           .fieldTypes
                           .find{case FieldType(fname, _) => fname == field}

        ftypeOpt match
            case Some(FieldType(_, fieldType)) => Right(fieldType) 
            case None => Left(RuntimeError.FieldNotFoundInProxy)
        
    def conformToFieldType(field: String, v : CESKValue, classDefs: ClassDefs): Either[RuntimeError, CESKValue] =
        for 
            ftype        <- getFieldType(field)
            conformedVal <- conformToType(v, ftype, classDefs)
        yield
            conformedVal

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







                
            
                
                
                
         