package linker

import ast._

object Synthesizer:

  def synthesizeSystem(s: CleanSystem): CleanSystem = 
    s match
      case System(modules, imports, progb) => 
        val topLevelModule = SystemToClassLinker.generateTopLevelModule(modules, imports)
        val topLevelScopedModules = topLevelModule.generateModuleToScopedModulesMap()

        val (newBodyModulesList, newBodyImports) = synthesizeImports(imports, topLevelModule, "Body")
        val (updModList, newModsCreated) = modules.foldLeft((List.empty[CleanTypedModule], List.empty[CleanTypedModule])){
          case ((modList, newModList), mod) =>
            mod match
              case Module.Untyped(mname, imports, clas) => 
                (modList, newModList)
              case tMod @ Module.Typed(mname, imports, clas, shape) =>
                val modDep = topLevelModule.findModuleInDAG(mname) match
                  case Some(mDep) => mDep
                  case None => throw new Exception("All modules should be reachable from base module")
                
                val (updTMod, newTModsCreated) = synthesizeModule(tMod, modDep)
                (modList.appended(updTMod),newModList ++ newTModsCreated)
        }

        System[Clean](
          newBodyModulesList ++ newModsCreated ++ updModList,
          newBodyImports,
          progb
        )


  /**
    * For a Typed Module, updates any timports to imports and creates Typed Modules.
    *
    * @param m the current module
    * @param modDep the module dependency for the module node
    * @return updated Typed Module with new imports, List of Typed Modules created for those new imports
    */
  def synthesizeModule(m: CleanTypedModule, modDep: ModuleDependency): (CleanTypedModule, List[CleanTypedModule]) =
    m match
      case Module.Typed(mname, imports, clas, shape) =>
        val (newModuleModulesList, newModuleModuleImports) = synthesizeImports(imports, modDep, mname)
        (
          Module.Typed[Clean](mname, newModuleModuleImports, clas, shape), 
          newModuleModulesList
        )

  /**
    * Creates the new List of Typed Modules from the timports in the scope of a Module using the ModuleDependency node of that module. 
    *
    * @param imports imports in the scope of this module
    * @param modDep module dependency node for the module
    * @param intoMName mname for renaming
    * @return the List of created Typed Modules and the list of original + updated imports
    */
  def synthesizeImports(imports: List[CleanImport], modDep: ModuleDependency, intoMName: String): (List[CleanTypedModule], List[CleanImport]) =
    imports.foldLeft((List.empty[CleanTypedModule], List.empty[CleanImport])){
      case ((modsList, impList), imp) =>
        synthesizeImport(imp, modDep, intoMName) match
          case (newImport, None) => (modsList, impList.appended(newImport))
          case (newImport, Some(tMod)) => (modsList.appended(tMod), impList.appended(newImport))
    }

  /**
    * Creates the TypedModule with updated import, otherwise returns the import and None
    *
    * @param i import to process
    * @param modDep module dependency in the scope of the current module NOT the module being imported
    * @param intoMName the mname to use for renaming
    * @return updated import or original import, typed module if created else None
    */
  def synthesizeImport(i: CleanImport, modDep: ModuleDependency, intoMName: String): (CleanImport, Option[CleanTypedModule]) = 
    i match
      case Import.Untyped(mname) => (Import.Untyped(mname), None)
      case Import.Typed(mname, importShape) =>
        val newTypedModName = s"$mname.into.$intoMName"
        modDep.findModuleInDAG(mname) match
          case Some(ModuleDependency(currMname, currClss, None, currDependencies)) => 
            
            val modulesImports: List[CleanImport] = currDependencies.map {
              case (ModuleDependency(depMname, depClss, depShape, depDependencies), depImportShape) =>
                depImportShape match
                  case Some(depImportedShape) => Import.Typed(depMname, depImportedShape) 
                  case None => Import.Untyped(depMname)
            }
            // Idt we need this blc a typed import is only for untyped modules
            // which will never themselves contain typed imports
            // modulesImports.foreach(imp => synthesizeImport(imp, modDep, currMname))

            // create the new typed module
            val newTypedMod: CleanTypedModule = Module.Typed[Clean](
              newTypedModName,
              modulesImports,
              currClss,
              importShape
            )

            (Import.Untyped[Clean](newTypedModName), Some(newTypedMod))
          case None => throw new Exception(s"All imported modules should be reachable and found in DAG $modDep")
          case _ => throw new Exception(s"Should never have typed import with typed module $i | $mname")
        
    