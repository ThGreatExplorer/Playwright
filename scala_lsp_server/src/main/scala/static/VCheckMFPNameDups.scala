package static

import ast._
import ast.ValidityErrNodes._
import util.{identifyNameDupsWErr, getFTypeNames, getMTypeNames, getMNames}
import ast.ConverterToWE.{importToWE, untypedImportToWE}

// MFP stands for Method, Field, Paramter
object VCheckMFPNameDups:

    // Module Valididty

    def mfpDupsSys(s: CleanRawSystem): RawSystemWE = s match
        case RawSystem(modules, imports, progb, range) =>
            WE.Node(RawSystem(
                modules.map(moduleDupsMFP),
                imports.map(importToWE),
                ConverterToWE.progBlockToWE(progb),
                range
            ))

    def moduleDupsMFP(m: CleanModule): ModuleWE = WE.Node(m match
        case Module(mname, imports, clas, range) =>
            Module(
                WE.Node(mname), 
                imports.map(importToWE), 
                classDupsMFP(clas), 
                range
            )
    )

    // Type Validity

    def typeDupsMFP(t: CleanType) : TypeWE = t match
        case Type.Number(range) => 
            WE.Node(Type.Number(range))
        case s @ Type.Shape(ftypes, mtypes, _) => 
            shapeDupsMFP(s)

    def shapeDupsMFP(s: CleanShapeType) : ShapeTypeWE = s match
        case Type.Shape(ftypes, mtypes, range) =>
            WE.Node(Type.Shape(
                ftypesDupsMFP(ftypes), 
                mtypesDupsMFP(mtypes),
                range
            ))
            
    def ftypesDupsMFP(ftypes: List[CleanFieldType]) : List[FieldTypeWE] = 
        val fieldNamesWE = ftypes.getFTypeNames.identifyNameDupsWErr(DuplicateFieldName)
        val ftypesAndNamesWE = ftypes.zip(fieldNamesWE) 

        ftypesAndNamesWE.map{ 
            case (FieldType(_, fieldType, range), fnameWE) => 
                WE.Node(FieldType(
                    fnameWE,
                    typeDupsMFP(fieldType),
                    range
                ))
        }

    def mtypesDupsMFP(mtypes: List[CleanMethodType]) : List[MethodTypeWE] = 
        val methodNamesWE = mtypes.getMTypeNames.identifyNameDupsWErr(DuplicateMethod)
        val mtypesAndNamesWE = mtypes.zip(methodNamesWE) 

        mtypesAndNamesWE.map{ 
            case (MethodType(_, paramTypes, retType, range), mnameWE) => 
                WE.Node(MethodType(
                    mnameWE,
                    paramTypes.map(typeDupsMFP),
                    typeDupsMFP(retType),
                    range
                ))
        }

    // Class Valididty

    def mfpDupsProg(p: CleanProgram): ProgramWE = p match
        case Program(clss, pblock, range) =>
            WE.Node(Program(
                clss.map(classDupsMFP),
                ConverterToWE.progBlockToWE(pblock),
                range
            ))

    def classDupsMFP(clss: CleanClass): ClassWE = clss match
        case Class(cname, fields, methods, shape, range) =>
            val shapeDupsWE = shape match
                case None => None
                case Some(shape) => Some(shapeDupsMFP(shape))
            
            WE.Node(Class(
                WE.Node(cname),
                fields.identifyNameDupsWErr(DuplicateFieldName),
                processMethodDups(methods),
                shapeDupsWE,
                range
            ))

    def processMethodDups(methods: List[CleanMethod]): List[MethodWE] =

        val methodNamesWE = methods.getMNames.identifyNameDupsWErr(DuplicateMethod)
        val methodsAndNamesWE = methods.zip(methodNamesWE) 

        methodsAndNamesWE.map{ 
            case (Method(_, params, pblock, range), mnameWE) => 
                WE.Node(Method(
                    mnameWE,
                    params.identifyNameDupsWErr(DuplicateParamName),
                    ConverterToWE.progBlockToWE(pblock),
                    range
                ))
        }
