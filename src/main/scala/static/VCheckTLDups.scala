package static

import ast._
import ast.ValidityErrNodes._
import util.identifyNameDupsWErr

object VCheckTLDups:

    // Module Valididty

    def moduleDupsSys(s: CleanSystem): SystemWE = s match
        case System(modules, imports, progb) => 
            WE.Node(System(
                moduleDupsModules(modules),
                imports.map(WE.Node(_)),
                ConverterToWE.progBlockToWE(progb)
            ))

    def moduleDupsModules(modules: List[CleanModule]): List[ModuleWE] =  

        val moduleNamesWE = modules.getMDNames.identifyNameDupsWErr(DuplicateModuleName)
        val modulesAndNamesWE = modules.zip(moduleNamesWE) 

        modulesAndNamesWE.map{ 
            case (Module(_, imports, clas), mnameWE) => 
                WE.Node(Module(
                    mnameWE,
                    imports.map(WE.Node(_)),
                    ConverterToWE.classToWE(clas)
                ))
        }

    // Class Valididty

    def classDupsProg(p: CleanProgram): ProgramWE = p match
        case Program(classes, progb) => 
            WE.Node(Program(
                classDupsClasses(classes),
                ConverterToWE.progBlockToWE(progb)
            ))

    def classDupsClasses(clss: List[CleanClass]): List[ClassWE] =  

        val classNamesWE = clss.getCNames.identifyNameDupsWErr(DuplicateClassName)
        val clssAndNamesWE = clss.zip(classNamesWE) 

        clssAndNamesWE.map{ 
            case (Class(_, fields, methods), cnameWE) => 
                WE.Node(Class(
                    cnameWE,
                    fields.map(WE.Node(_)),
                    methods.map(ConverterToWE.methodToWE)
                ))
        }
