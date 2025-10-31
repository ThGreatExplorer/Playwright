package static

import ast._
import ast.ValidityErrNodes._
import util.identifyNameDupsWErr

// MFP stands for Method, Field, Paramter
object VCheckMFPNameDups:

    // Module Valididty

    def mfpDupsSys(s: CleanSystem): SystemWE = s match
        case System(modules, imports, progb) =>
            WE.Node(System(
                modules.map(moduleDupsMFP),
                imports.map(WE.Node(_)),
                ConverterToWE.progBlockToWE(progb)
            ))

    def moduleDupsMFP(m: CleanModule): ModuleWE = m match
        case Module(mname, imports, clas) =>
            WE.Node(Module(
                WE.Node(mname), 
                imports.map(WE.Node(_)), 
                classDupsMFP(clas)
            ))

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