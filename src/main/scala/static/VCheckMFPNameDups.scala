package static

import ast._
import ast.ValidityErrNodes._
import util.{identifyNameDupsWErr, getFTypeNames, getMTypeNames, getMNames}
import ast.ConverterToWE.{importToWE, untypedImportToWE}

// MFP stands for Method, Field, Paramter
object VCheckMFPNameDups:

    // Module Valididty

    def mfpDupsSys(s: CleanSystem): SystemWE = s match
        case System(modules, imports, progb) =>
            WE.Node(System(
                modules.map(moduleDupsMFP),
                imports.map(importToWE),
                ConverterToWE.progBlockToWE(progb)
            ))

    def moduleDupsMFP(m: CleanModule): ModuleWE = WE.Node(m match
        case Module.Typed(mname, imports, clas, shape) =>
            Module.Typed(
                WE.Node(mname), 
                imports.map(importToWE), 
                classDupsMFP(clas),
                shapeDupsMFP(shape)
            )
        case Module.Untyped(mname, imports, clas) =>
            Module.Untyped(
                WE.Node(mname),
                imports.map(untypedImportToWE),
                classDupsMFP(clas)
            )
    )

    // Type Validity

    def typeDupsMFP(t: CleanType) : TypeWE = t match
        case Type.Number() => 
            WE.Node(Type.Number())
        case s @ Type.Shape(ftypes, mtypes) => 
            shapeDupsMFP(s)

    def shapeDupsMFP(s: CleanShapeType) : ShapeTypeWE = s match
        case Type.Shape(ftypes, mtypes) =>
            WE.Node(Type.Shape(
                ftypesDupsMFP(ftypes), 
                mtypesDupsMFP(mtypes)
            ))
            
    def ftypesDupsMFP(ftypes: List[CleanFieldType]) : List[FieldTypeWE] = 
        val fieldNamesWE = ftypes.getFTypeNames.identifyNameDupsWErr(DuplicateFieldName)
        val ftypesAndNamesWE = ftypes.zip(fieldNamesWE) 

        ftypesAndNamesWE.map{ 
            case (FieldType(_, fieldType), fnameWE) => 
                WE.Node(FieldType(
                    fnameWE,
                    typeDupsMFP(fieldType)
                ))
        }

    def mtypesDupsMFP(mtypes: List[CleanMethodType]) : List[MethodTypeWE] = 
        val methodNamesWE = mtypes.getMTypeNames.identifyNameDupsWErr(DuplicateMethod)
        val mtypesAndNamesWE = mtypes.zip(methodNamesWE) 

        mtypesAndNamesWE.map{ 
            case (MethodType(_, paramTypes, retType), mnameWE) => 
                WE.Node(MethodType(
                    mnameWE,
                    paramTypes.map(typeDupsMFP),
                    typeDupsMFP(retType)
                ))
        }

    // Class Valididty

    def mfpDupsProg(p: CleanProgram): ProgramWE = p match
        case Program(clss, pblock) =>
            WE.Node(Program(
                clss.map(classDupsMFP),
                ConverterToWE.progBlockToWE(pblock)
            ))

    def classDupsMFP(clss: CleanClass): ClassWE = clss match
        case Class(cname, fields, methods) =>
            WE.Node(Class(
                WE.Node(cname),
                fields.identifyNameDupsWErr(DuplicateFieldName),
                processMethodDups(methods)
            ))

    def processMethodDups(methods: List[CleanMethod]): List[MethodWE] =

        val methodNamesWE = methods.getMNames.identifyNameDupsWErr(DuplicateMethod)
        val methodsAndNamesWE = methods.zip(methodNamesWE) 

        methodsAndNamesWE.map{ 
            case (Method(_, params, pblock), mnameWE) => 
                WE.Node(Method(
                    mnameWE,
                    params.identifyNameDupsWErr(DuplicateParamName),
                    ConverterToWE.progBlockToWE(pblock)
                ))
        }