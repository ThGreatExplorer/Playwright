package static

import ast._
import ast.ValidityErrNodes._
import ast.ConverterToWE.{shapeToWE, progBlockToWE, classToWE, untypedImportToWE, importToWE}
import util.{identifyNameDupsWErr, getFTypeNames, getMTypeNames, getMNames}
import util.ExampleKeyword

// MFP stands for Method, Field, Paramter
object VCheckImports:

    // Top Level entry point

    def checkImportsSys(sys: CleanSystem): SystemWE = WE.Node(sys match
        case System[Clean](modules, imports, progb, moduleData) =>

            val moduleData = ModuleData(modules)

            System(
                checkImportsModules(modules, moduleData),
                checkMixedImports(imports, moduleData.atTopLevel),
                progBlockToWE(progb), 
                moduleData
            )
    )

    // Module Valididty

    type ModToOptShapeMap = Map[String, Option[CleanShapeType]]

    def checkImportsModules(mods: List[CleanModule], moduleData : ModuleData) : List[ModuleWE] = 

        def checkImportsOneMod(m : CleanModule) : ModuleWE = m match
            case Module.Untyped(mname, imports, clas) => 
                WE.Node(Module.Untyped(
                    WE.Node(mname),
                    imports.map(untypedImportToWE(_)),
                    classToWE(clas)
                ))

            case Module.Typed(mname, imports, clas, shape) => 
                WE.Node(Module.Typed(
                    WE.Node(mname),
                    checkMixedImports(imports, moduleData.scopedAt(mname)),
                    classToWE(clas),
                    shapeToWE(shape)
                ))

        def checkImportsModsLoop(
            modsRem: List[CleanModule], modsSoFar: List[ModuleWE]
        ) : List[ModuleWE] = modsRem match

            case Nil => modsSoFar.reverse

            case (m: CleanModule) :: tail => 
                val processedModule = checkImportsOneMod(m)
                checkImportsModsLoop(tail, processedModule :: modsSoFar)


        checkImportsModsLoop(mods, Nil)

    def checkMixedImports(
        imports: List[CleanImport], moduleData : ScopedModuleData
    ) : List[ImportWE] =

        // Used to ensure we do not import the same module with two different shapes
        type ModToShapeMap = Map[String, CleanShapeType]

        // Assumes that all imports are defined and distinct 
        def checkOneImport(imp : CleanImport, importedSoFar : ModToShapeMap) : (ImportWE, ModToShapeMap) = 
            val mname = imp.importedModName
            (imp, moduleData.lookupModuleShape(mname)) match
                // Untyped import of a typed module into a typed module
                case (Import.Untyped(mname), Some(shape)) => 
                    (importToWE(imp), importedSoFar.updated(mname, shape))
                // Untyped import of an untyped module into a typed module
                case (Import.Untyped(_), None) =>
                    (WE.Err(UntypedModImportedWithoutTImport), importedSoFar)
                
                // Typed import of a typed module into a typed module
                case (Import.Typed(_, _), Some(shape)) => 
                    (WE.Err(TypedModTImported), importedSoFar)
                // Typed import of an untyped module into a typed module 
                case (Import.Typed(mname, importedShape), None) => 
                    importedSoFar.get(mname) match
                        // Not seen before
                        case None => 
                            (importToWE(imp), importedSoFar.updated(mname, importedShape))
                        // Seen before (imports are distinct, so this means a different shape was imported)
                        case Some(alreadyImportedShape) =>  
                            if importedShape == alreadyImportedShape then
                                (importToWE(imp), importedSoFar)
                            else 
                                (WE.Err(UntypedModTImportedWithDiffShape), importedSoFar)

        def checkMixedImportsLoop(
            impsRem: List[CleanImport], impsSoFar: List[ImportWE], importedSoFar : ModToShapeMap
        ) : List[ImportWE] = impsRem match

            case Nil => impsSoFar.reverse

            case (imp: CleanImport) :: tail => 

                // Due to spec mistake this pass occurs before Undefined checking, so
                // we simply pass undefined imports through at this stage
                val (processedImport, updImportedSoFar) = 
                    if moduleData.contains(imp.importedModName) then
                        checkOneImport(imp, importedSoFar)
                    else 
                        (importToWE(imp), importedSoFar) 

                checkMixedImportsLoop(tail, processedImport :: impsSoFar, updImportedSoFar)

        checkMixedImportsLoop(imports, Nil, Map.empty)
