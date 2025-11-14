package static

import ast._
import ast.ValidityErrNodes._
import ast.ConverterToWE.{shapeToWE, progBlockToWE, classToWE, untypedImportToWE, importToWE}
import util.{identifyNameDupsWErr, getFTypeNames, getMTypeNames, getMNames}
import util.ExampleKeyword

// MFP stands for Method, Field, Paramter
object VCheckImports:

    // Top Level entry point

    def checkImportsSys(s: CleanSystem): SystemWE = WE.Node(s match
        case System[Clean](modules, imports, progb) =>

            val (checkedModules, modToOptShapeMap) = checkImportsModules(modules)

            System(
                checkedModules,
                checkMixedImports(imports, modToOptShapeMap),
                progBlockToWE(progb)
            )
    )

    // Module Valididty

    type ModToOptShapeMap = Map[String, Option[CleanShapeType]]

    def checkImportsModules(mods: List[CleanModule]) : (List[ModuleWE], ModToOptShapeMap) = 

        def checkImportsOneMod(m : CleanModule, mapSoFar : ModToOptShapeMap) : (ModuleWE, ModToOptShapeMap) = m match
            case Module.Untyped(mname, imports, clas) => 
                val processedModule = WE.Node(Module.Untyped(
                    WE.Node(mname),
                    imports.map(untypedImportToWE(_)),
                    classToWE(clas)
                ))
                (processedModule, mapSoFar.updated(mname, None))

            case Module.Typed(mname, imports, clas, shape) => 
                val processedModule = WE.Node(Module.Typed(
                    WE.Node(mname),
                    checkMixedImports(imports, mapSoFar),
                    classToWE(clas),
                    shapeToWE(shape)
                ))
                (processedModule, mapSoFar.updated(mname, Some(shape)))

        def checkImportsModsLoop(
            modsRem: List[CleanModule], modsSoFar: List[ModuleWE], modToOptShapeMapSoFar : ModToOptShapeMap
        ) : (List[ModuleWE], ModToOptShapeMap) = modsRem match

            case Nil => (modsSoFar.reverse, modToOptShapeMapSoFar)

            case (m: CleanModule) :: tail => 
                val (processedModule, updModToOptShapeMap) = checkImportsOneMod(m, modToOptShapeMapSoFar)
                checkImportsModsLoop(tail, processedModule :: modsSoFar, updModToOptShapeMap)


        checkImportsModsLoop(mods, Nil, Map.empty)

    def checkMixedImports(
        imports: List[CleanImport], definedModsToOptShapeMap : ModToOptShapeMap
    ) : List[ImportWE] =

        // Used to ensure we do not import the same module with two different shapes
        type ModToShapeMap = Map[String, CleanShapeType]

        def checkOneImport(i : CleanImport, importedSoFar : ModToShapeMap) : (ImportWE, ModToShapeMap) = i match
            case imp @ Import.Untyped(mname) =>
                definedModsToOptShapeMap.get(mname) match 
                    // Undefined module import
                    case None => (untypedImportToWE(imp), importedSoFar)
                    // Untyped import of an untyped module into a typed module
                    case Some(None) => (WE.Err(UntypedModImportedWithoutTImport), importedSoFar)
                    // Untyped import of a typed module into a typed module
                    case Some(Some(shape)) => (untypedImportToWE(imp), importedSoFar.updated(mname, shape))

            case imp @ Import.Typed(mname, importedShape) =>
                (definedModsToOptShapeMap.get(mname), importedSoFar.get(mname)) match 
                    // Undefined module import
                    case (None, _) => (importToWE(imp), importedSoFar)
                    // Typed import of a typed module into a typed module
                    case (Some(Some(shape)), _) => (WE.Err(TypedModTImported), importedSoFar)
                    // Typed import of an untyped module into a typed module NOT SEEN before
                    case (Some(None), None) => 
                        (importToWE(imp), importedSoFar.updated(mname, importedShape))
                    // Typed import of an untyped module into a typed module SEEN before
                    case (Some(None), Some(alreadyImportedShape)) => 
                        if importedShape == alreadyImportedShape then
                            (importToWE(imp), importedSoFar)
                        else 
                            (WE.Err(UntypedModTImportedWithDiffShape), importedSoFar)

        def checkMixedImportsLoop(
            impsRem: List[CleanImport], impsSoFar: List[ImportWE], importedSoFar : ModToShapeMap
        ) : List[ImportWE] = impsRem match

            case Nil => impsSoFar.reverse

            case (imp: CleanImport) :: tail => 
                val (processedImport, updImportedSoFar) = checkOneImport(imp, importedSoFar)
                checkMixedImportsLoop(tail, processedImport :: impsSoFar, updImportedSoFar)


        checkMixedImportsLoop(imports, Nil, Map.empty)
