package static

import ast._
import main.main
import util.{getClasses}


object SystemToClassLinker:

  def modulesToModulesInScope(modules: List[CleanModule]): Map[String, Set[CleanModule]] = 
    val (modulesInScopeMap, _) = modules.foldLeft((Map.empty[String, Set[CleanModule]], Set.empty[CleanModule])) {
      case ((accMap, accSet), module) => 
        (accMap.updated(module.mname, accSet), accSet + module)
    }
    modulesInScopeMap

  def trimUnreachableStates(s: CleanSystem, reachableModules: Set[String]): CleanSystem =
    s match
      case System(modules, imports, progb) =>
        val modulesReachable = modules.filter(module => reachableModules.contains(module.mname))
        System(
          modulesReachable,
          imports,
          progb
        )
    
  def generateBaseModule(mods: List[CleanModule], imports: List[CleanImportedMod]): ModuleDependency = 
    val modulesInScopeMap = modulesToModulesInScope(mods)
    val baseModName = "#Base@"
    // all modules are in scope for the base module to import
    ModuleDependency(baseModName, Class(baseModName, List.empty, List.empty), None, modulesInScopeMap.updated(baseModName, mods.toSet), imports)

  /**
    * Renames all occurrences of classes in a given Clean System into 
    * ModuleName.ClassName as the first step of linking modules into classes.
    *
    * @param s the clean System to convert
    * @return base Module representing the module dependency graph and 
    * the renamed System
    */
  def renameClassesUsingDependencyGraph(s: CleanSystem): (ModuleDependency, CleanSystem) =
    s match
      case System(modules, imports, progb) =>
        val baseModule = generateBaseModule(modules, imports)
        val trimmedSys = trimUnreachableStates(s, baseModule.findReachableModules())
        (baseModule, SystemToClassRenamerAST.renameSystem(trimmedSys, baseModule))

  /**
    * Drops the import and module statements as part of the second step of
    * linking System to Module programs.
    *
    * @param s
    * @return the converted Clean Program
    */
  def convertModulesToClasses(s: CleanSystem): CleanProgram =
    s match
      case System(modules, imports, progb) =>
        Program(
          modules.getClasses,
          progb
        )