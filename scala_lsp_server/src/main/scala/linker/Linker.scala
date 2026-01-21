package linker

import ast._
import util.{getClasses}
import linker.ModuleDependency
import ast.Type.Shape
import sexprs.{Position, Range}


object SystemToClassLinker:

  // Synthetic range for the Dummy Base Module
  private val syntheticRange = Range(Position(0, 0), Position(0, 0))

  def modulesToModulesInScope(modules: List[CleanModule]): Map[String, Set[CleanModule]] = 
    val (modulesInScopeMap, _) = modules.foldLeft((Map.empty[String, Set[CleanModule]], Set.empty[CleanModule])) {
      case ((accMap, accSet), module) => 
        module match
          case Module(mname, imports, clas, _) =>
            (accMap.updated(mname, accSet), accSet + module)
    }
    modulesInScopeMap

  def trimUnreachableStates(s: CleanSystem, reachableModules: Set[String]): CleanSystem =
    s match
      case System(modules, imports, progb, modData, range) =>
        val modulesReachable = modules.filter(
          module => 
            module match
              case Module(mname, imports, clas, _) => 
                reachableModules.contains(mname)
        )
        System(
          modulesReachable,
          imports,
          progb, 
          modData,
          range
        )
    
  def generateTopLevelModule(mods: List[CleanModule], imports: List[CleanImport]): ModuleDependency = 
    val modulesInScopeMap = modulesToModulesInScope(mods)
    val topLevelModName = "#topLevelModName@"
    // all modules are in scope for the top level module to import
    ModuleDependency(
      topLevelModName,
      Class[Clean](topLevelModName, List.empty, List.empty, None, syntheticRange),
      modulesInScopeMap.updated(topLevelModName, mods.toSet),
      imports
    )

  /**
    * Renames all occurrences of classes in a given Clean System into 
    * ModuleName.ClassName as the first step of linking modules into classes.
    *
    * @param s the clean System to convert
    * @return the renamed System
    */
  def renameClassesUsingDependencyGraph(s: CleanSystem): CleanSystem =
    s match
      case System(modules, imports, progb, modData, _) =>
        val topLevelModule = generateTopLevelModule(modules, imports)
        val trimmedSys = trimUnreachableStates(s, topLevelModule.findReachableModules())
        SystemToClassRenamerAST.renameSystem(trimmedSys, topLevelModule)

  /**
    * Drops the import and module statements as part of the second step of
    * linking System to Module programs.
    *
    * @param s
    * @return the converted Clean Program
    */
  def convertModulesToClasses(s: CleanSystem): CleanProgram =
    s match
      case System(modules, imports, progb, modData, _) =>
        Program(
          modules.getClasses,
          progb,
          progb.range
        )

  def linkProgram(s : CleanSystem) : CleanProgram = 
    val renamedSys = renameClassesUsingDependencyGraph(s)
    val classProg  = convertModulesToClasses(renamedSys)
    classProg
