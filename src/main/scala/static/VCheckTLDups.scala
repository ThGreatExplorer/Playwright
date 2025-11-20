package static

import ast._
import ast.ValidityErrNodes._
import ast.ConverterToWE.{importToWE, untypedImportToWE, shapeToWE, progBlockToWE, classToWE, methodToWE}
import util.{identifyNameDupsWErr, getMDNames, getCNames}

object VCheckTLDups:

    // Module Valididty

    def moduleDupsSys(s: CleanRawSystem): RawSystemWE = s match
        case RawSystem(modules, imports, progb) => 
            WE.Node(RawSystem(
                moduleDupsModules(modules),
                imports.map(importToWE),
                progBlockToWE(progb)
            ))

    def moduleDupsModules(modules: List[CleanModule]): List[ModuleWE] =  

        val moduleNamesWE = modules.getMDNames.identifyNameDupsWErr(DuplicateModuleName)
        val modulesAndNamesWE = modules.zip(moduleNamesWE) 

        modulesAndNamesWE.map{ 
            case (Module.Typed(_, imports, clas, shape), mnameWE) => 
                WE.Node(Module.Typed(
                    mnameWE,
                    imports.map(importToWE),
                    classToWE(clas),
                    shapeToWE(shape)
                ))
            case (Module.Untyped(_, imports, clas), mnameWE) =>
                WE.Node(Module.Untyped(
                    mnameWE,
                    imports.map(untypedImportToWE),
                    classToWE(clas)
                ))
        }

    // Class Valididty

    def classDupsProg(p: CleanProgram): ProgramWE = p match
        case Program(classes, progb) => 
            WE.Node(Program(
                classDupsClasses(classes),
                progBlockToWE(progb)
            ))

    def classDupsClasses(clss: List[CleanClass]): List[ClassWE] =  

        val classNamesWE = clss.getCNames.identifyNameDupsWErr(DuplicateClassName)
        val clssAndNamesWE = clss.zip(classNamesWE) 

        clssAndNamesWE.map{ 
            case (Class(_, fields, methods), cnameWE) => 
                WE.Node(Class(
                    cnameWE,
                    fields.map(WE.Node(_)),
                    methods.map(methodToWE)
                ))
        }
