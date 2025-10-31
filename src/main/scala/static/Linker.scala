package static

import ast._
import scala.collection.mutable.Map as MutableMap
import main.main

final case class ModuleDependency(
  mname: String,
  clss: CleanClass,
  dependencies: List[ModuleDependency]
):

  /**
    * Walks the DAG to collect all the reachable modules
    *
    * @return the set of module names reachable
    */
  def findReachableModules(): Set[String] =
    findReachableModulesHelper(Set.empty)
  
  private def findReachableModulesHelper(nodesAcc: Set[String]): Set[String] = 
      if nodesAcc.contains(mname) then
        nodesAcc
      else 
        val updatedAcc = nodesAcc + mname
        dependencies.foldLeft(updatedAcc)((acc, dep) => 
          dep.findReachableModulesHelper(acc)
        )
  
  /**
    * Gets the module dependency if is reachable in this DAG, 
    * returning it as an Option.
    *
    * @param mnameToFind the module name to find
    * @return Some(ModuleDependency) if exists otherwise None
    */
  def getModule(mnameToFind: String): Option[ModuleDependency] =
    getModuleHelper(mnameToFind, Set.empty)
  
  private def getModuleHelper(mnameToFind: String, visited: Set[String]): Option[ModuleDependency] =
    if visited.contains(mname) then
      None
    else
      val updatedVisited = visited + mname
      
      if mname == mnameToFind then
        Some(this)
      else
        dependencies.find { dep =>
          dep.getModuleHelper(mnameToFind, updatedVisited) match
            case Some(module) => true
            case None => false
        }

  /**
    * Generates the Rename Map for the module in this scope by traversing
    * its dependencies in the order imported.
    *
    * @return the map from class names to their renamed Class Name
    */
  def generateRenameMap(): Map[String, String] =
    val renameMap = this.dependencies.foldLeft(Map.empty[String,String])((acc, dependency) =>
      dependency match
        case ModuleDependency(depMname, depClss, depDependencies) =>
          acc.updated(depClss.cname, f"$depMname.${depClss.cname}")
    )
    val cname = clss.cname
    renameMap.updated(cname, f"$mname.$cname")

  override def toString: String = 
    val (result, visited) = toStringHelper("", "", Set.empty)
    result

  private def toStringHelper(prefix: String, childPrefix: String, visited: Set[String]): (String, Set[String]) =
    val result = new StringBuilder()
    result.append(prefix).append(mname)
    
    if visited.contains(mname) then
      result.append(" (already shown)")
      (result.toString(), visited)
    else
      val updatedVisited = visited + mname
      
      val (finalResult, finalVisited) = dependencies.zipWithIndex.foldLeft((result.toString, updatedVisited)) {
        case ((accResult, accVisited), (dep, index)) =>
          val isLast = index == dependencies.length - 1
          val currentPrefix = if isLast then childPrefix + "└── " else childPrefix + "├── "
          val nextChildPrefix = if isLast then childPrefix + "    " else childPrefix + "│   "
          
          val (depString, newVisited) = dep.toStringHelper(currentPrefix, nextChildPrefix, accVisited)
          (accResult + "\n" + depString, newVisited)
      }
    
      (finalResult, finalVisited)

object ModuleDependency:

  /**
    * Constructs a DAG Module Dependency graph by searching through each
    * module in import order and then recurses for each import statement 
    * returning the module if already constructed otherwise creating a new module.
    * 
    * Since we are guaranteed by the validity checker that all modules and imports
    * have correct scope, there shouldn't be any cycles or throws 
    * from the importToModule method nor from the modules in scope lookup.
    * 
    * @param mname the module name of the current module
    * @param clas the class name of the current moduled
    * @param modulesInScopeMap map from module to modules available for import (based on hierachical scope)
    * @param imports the list of imports for the current module
    * @return
    */
  def apply(mname: String, clas: CleanClass, modulesInScopeMap: Map[String, Set[CleanModule]], imports: List[CleanImportedMod]): ModuleDependency =
    buildDAG(mname, clas, modulesInScopeMap, imports, MutableMap.empty)

  private def buildDAG(
    mname: String,
    clas: CleanClass, 
    modulesInScopeMap: Map[String, Set[CleanModule]],
    imports: List[CleanImportedMod],
    memoization: MutableMap[String, ModuleDependency]
  ): ModuleDependency =
    
    // Check if we've already built this module's dependency tree
    memoization.get(mname) match
      case Some(existing) => existing
      case None =>
        // Build dependencies first, then memoize to handle potential cycles
        val dependencies = imports.map { imp =>
          importToModule(modulesInScopeMap(mname), imp) match
            case Module(importedMname, importedImports, importedClas) => 
              buildDAG(importedMname, importedClas, modulesInScopeMap, importedImports, memoization)
        }
        
        val result = ModuleDependency(mname, clas, dependencies)
        memoization.update(mname, result)
        result

  def importToModule(modules: Set[CleanModule], imp: CleanImportedMod): CleanModule =
    modules.find(module => module.mname == imp) match
      case None => throw new Exception(f"Invalid import $imp in $modules")
      case Some(module) => module 


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
        val modulesInScopeMap = modulesToModulesInScope(modules)
        val baseModName = "#Base@"
        // all modules are in scope for the base module to import
        val baseModule = ModuleDependency(baseModName, Class(baseModName, List.empty, List.empty), modulesInScopeMap.updated(baseModName, modules.toSet), imports)
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
          modules.map(module => module.clas),
          progb
        )