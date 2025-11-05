package static

import ast._
import ast.ValidityErrNodes._
import ast.ConverterToWE.{shapeToWE, progBlockToWE, classToWE, methodToWE}
import util.{identifyNameDupsWErr, getMDNames, getCNames}

object VCheckTLDups:

    // Module Valididty

    def moduleDupsSys(s: CleanSystem): SystemWE = s match
        case System(modules, imports, progb) => 
            WE.Node(System(
                moduleDupsModules(modules),
                imports.map(WE.Node(_)),
                progBlockToWE(progb)
            ))

    def moduleDupsModules(modules: List[CleanModule]): List[ModuleWE] =  

        val moduleNamesWE = modules.getMDNames.identifyNameDupsWErr(DuplicateModuleName)
        val modulesAndNamesWE = modules.zip(moduleNamesWE) 

        modulesAndNamesWE.map{ 
            case (Module(_, imports, clas, shape), mnameWE) => 
                WE.Node(Module(
                    mnameWE,
                    imports.map(WE.Node(_)),
                    classToWE(clas),
                    shape.map(shapeToWE)
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
