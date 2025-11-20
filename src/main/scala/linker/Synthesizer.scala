package linker

import ast._
import static.{ModuleData,ModuleDataEntry}
import util.getMDNames

object Synthesizer:

  def synthesizeSystem(sys: CleanSystem): CleanSystem = sys match
    case System(modules, imports, progb) => 

      val moduleData = ModuleData(sys)
      val updModules = synthesizeModules(modules, moduleData)
      val (newTypedCopiesOfMods, updImports) = synthesizeImports(imports, "Body", moduleData)
      
      System[Clean](
        updModules ::: newTypedCopiesOfMods,
        updImports,
        progb
      )

  // Special entry point for Assignment 11
  def synthesizeAndGetMNames(sys: CleanSystem): List[String] = sys match
    case System(modules, imports, progb) => 

      val moduleData = ModuleData(sys)
      val updModules = synthesizeModules(modules, moduleData)
      val (newTypedCopiesOfMods, updImports) = synthesizeImports(imports, "Body", moduleData)
      val allModules = updModules ::: newTypedCopiesOfMods
      
      allModules.getMDNames

  /**
    * Performs a synthesis run over a top level list of modules. Untyped modules are passed 
    * through as-is, Typed Modules are checked for Typed Imports. If a Typed imports if found, 
    * we create a Typed copy of this Untyped module with the shape specified in the Typed import.
    *
    * @param mods list of Modules to be processed in the scope of this module
    * @param moduleData Module Data map for quick lookups
    * @return the List of Modules that includes the newly created Typed copies of Untyped Modules
    * imported into Typed Modules
    */
  def synthesizeModules(mods: List[CleanModule], moduleData : ModuleData) : List[CleanModule] = 

    def synthesizeModule(m: CleanModule): (CleanModule, List[CleanModule]) = m match
      case mod @ Module.Untyped(mname, imports, clas) => 
        (mod, Nil)

      case Module.Typed(mname, imports, clas, shape) =>
        val (newTypedCopiesOfMods, updImports) = synthesizeImports(imports, mname, moduleData)
        (
          Module.Typed[Clean](mname, updImports, clas, shape), 
          newTypedCopiesOfMods
        )

    def synthesizeModulesLoop(
      modsRem: List[CleanModule], modsSoFar: List[CleanModule]
    ) : List[CleanModule] = modsRem match

        case Nil => modsSoFar.reverse

        case (module: CleanModule) :: tail => 
          val (processedModule, newTypedCopiesOfMods) = synthesizeModule(module)
          synthesizeModulesLoop(tail, processedModule :: newTypedCopiesOfMods ::: modsSoFar)

    synthesizeModulesLoop(mods, Nil)

  /**
    * Creates a List of Typed copies of Untyped Modules from the timports in the scope of the
    * current Module
    *
    * @param imports imports in the scope of this module
    * @param intoMName Module Name of the Current Module
    * @param moduleData Module Data map for quick lookups
    * @return the List of created Typed Modules and the list of updated imports
    */
  def synthesizeImports(
    imports: List[CleanImport], intoMName: String, moduleData : ModuleData
  ): (List[CleanModule], List[CleanImport]) =

    def synthesizeImport(imp: CleanImport): (CleanImport, Option[CleanModule]) = imp match
      case Import.Untyped(mname) => (Import.Untyped(mname), None)

      case Import.Typed(mname, importShape) =>
        moduleData.lookupModule(mname) match
          case ModuleDataEntry(imports, currClss, None) => 
            
            val newTypedModName = s"$mname.into.$intoMName"

            val newTypedMod: CleanModule = Module.Typed[Clean](
              newTypedModName,
              imports,
              currClss,
              importShape
            )

            (Import.Untyped[Clean](newTypedModName), Some(newTypedMod))

          case _ => 
            throw new Exception(s"Should never happen: Typed import of Typed module $imp | $mname")

    def synthesizeImportsLoop(
      impsRem: List[CleanImport], newTypedModsSoFar: List[CleanModule], impsSoFar: List[CleanImport]
    ) : (List[CleanModule], List[CleanImport]) = impsRem match

      case Nil => (newTypedModsSoFar.reverse, impsSoFar.reverse)

      case (imp: CleanImport) :: tail => 
        val (processedImp, newTypedModOpt) = synthesizeImport(imp)
        val newTypedMods = 
          newTypedModOpt match
            case Some(newMod) => newMod :: newTypedModsSoFar
            case None         => newTypedModsSoFar

        synthesizeImportsLoop(tail, newTypedMods, processedImp :: impsSoFar)

    synthesizeImportsLoop(imports, Nil, Nil)
