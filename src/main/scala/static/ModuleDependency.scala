package static

import ast._
import scala.annotation.tailrec
import scala.collection.mutable.Map as MutableMap

final case class ModuleDependency(
  mname: String,
  clss: CleanClass,
  shape: Option[CleanShapeType],
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
    * Gets the module dependency if is reachable from the current node in this DAG, 
    * returning it as an Option.
    *
    * @param mnameToFind the module name to find
    * @return Some(ModuleDependency) if exists otherwise None
    */
  def findModuleInDAG(mnameToFind: String): Option[ModuleDependency] =

    @tailrec
    def searchSubTreeForDependency(dependencies: List[ModuleDependency], visited: Set[String]): Option[ModuleDependency] = {
      dependencies match {
        case Nil => None
        case head :: tail =>
          if visited.contains(head.mname) then 
            searchSubTreeForDependency(tail, visited)
          else if (head.mname == mnameToFind) 
            Some(head)
          else 
            val updatedVisited = visited + head.mname
            searchSubTreeForDependency(tail ++ head.dependencies, updatedVisited)
      }
    }

    searchSubTreeForDependency(List(this), Set.empty)

  /**
    * Generates the Rename Map for the module in this scope by traversing
    * its dependencies in the order imported.
    *
    * @return the map from class names to their renamed Class Name
    */
  def generateRenameMap(): Map[String, String] =
    val renameMap = this.dependencies.foldLeft(Map.empty[String,String])((acc, dependency) =>
      dependency match
        case ModuleDependency(depMname, depClss, _, _) =>
          val (cname, qualifiedName) = generatedQualifiedName(depMname, depClss)
          acc.updated(cname, qualifiedName)
    )
    val (cname, qualifiedName) = generatedQualifiedName(mname, clss)
    renameMap.updated(cname, qualifiedName)

  private def generatedQualifiedName(mname: String, clss: CleanClass): (String, String) =
    (clss.cname, f"$mname.${clss.cname}")

  /**
    * Generates the SClasses Map with (this, thisShape) for the module by traversing its 
    * dependencies in the order imported. Maps the class name to the CleanShapeType supplied 
    * in the module (if it exists)
    *
    * @return class names to their clean shape types
    */
  def generateSClassesMap(): Map[String, CleanShapeType] = 
    val sClassesMap = this.dependencies.foldLeft(Map.empty[String,CleanShapeType])((acc, dependency) => 
        dependency match
          case ModuleDependency(_, depClss, depShape, _) =>
            depShape match
              case None => acc
              case Some(actualShape) => acc.updated(clss.cname, actualShape)
    )
    this.shape match
      case None => sClassesMap
      case Some(actualShape) => sClassesMap.updated("this", actualShape)

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
  def apply(mname: String, clas: CleanClass, shape: Option[CleanShapeType], modulesInScopeMap: Map[String, Set[CleanModule]], imports: List[CleanImportedMod]): ModuleDependency =
    buildDAG(mname, clas, shape, modulesInScopeMap, imports, MutableMap.empty)

  private def buildDAG(
    mname: String,
    clas: CleanClass, 
    shape: Option[CleanShapeType],
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
            case Module(importedMname, importedImports, importedClas, importedShape) => 
              buildDAG(importedMname, importedClas, importedShape, modulesInScopeMap, importedImports, memoization)
        }
        
        val result = ModuleDependency(mname, clas, shape, dependencies)
        memoization.update(mname, result)
        result

  def importToModule(modules: Set[CleanModule], imp: CleanImportedMod): CleanModule =
    modules.find(module => module.mname == imp) match
      case None => throw new Exception(f"Invalid import $imp in $modules")
      case Some(module) => module 
