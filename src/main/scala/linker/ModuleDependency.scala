package linker

import ast._
import scala.annotation.tailrec
import scala.collection.mutable.Map as MutableMap

type ScopedModuleMap = Map[String, (CleanClass, Option[CleanShapeType])]
type ModuleToScopedModuleMap = Map[(String, Option[CleanShapeType]), ScopedModuleMap]

// Note that we can assume modules are deduplicated so each module name is unique
final case class ModuleDependency(
  mname: String,
  clss: CleanClass,
  shape: Option[CleanShapeType],
  dependencies: List[(ModuleDependency, Option[CleanShapeType])]
):

  /**
  * Generates a map from each reachable module to its scoped module map.
  * 
  * That is:
  *   Map[(ModuleName, Shape) -> Map[ImportedModuleName -> (Class, Shape)]]
  *
  * Each module's scoped map is produced using its generateScopedModules(),
  * and we recursively apply this to dependencies while memoizing.
  */
  def generateModuleToScopedModulesMap(): ModuleToScopedModuleMap =
    generateModuleToScopedModulesMapHelper(Map.empty)
    

  private def generateModuleToScopedModulesMapHelper(
    moduleToScopedModulesSoFar: ModuleToScopedModuleMap
  ): ModuleToScopedModuleMap =
    val key = (this.mname, this.shape)

    if moduleToScopedModulesSoFar.contains(key) then moduleToScopedModulesSoFar
    else
      val scoped = this.generateScopedModules()
      val updatedAcc = moduleToScopedModulesSoFar.updated(key, scoped)
      this.dependencies.foldLeft(updatedAcc) {
        case (acc, (childDep, _)) =>
          childDep.generateModuleToScopedModulesMapHelper(acc)
      }

  def generateScopedModules(): ScopedModuleMap =
    val scopeMap: ScopedModuleMap = this.dependencies.foldLeft(Map.empty){
      case (acc, (mod, shape)) => 
        (mod, shape) match
          case (ModuleDependency(mname, clss, Some(modShape), dependencies), None) =>
            acc.updated(mname, (clss, Some(modShape)))
          case (ModuleDependency(mname, clss, None, dependencies), Some(importShape)) =>
            acc.updated(mname, (clss, Some(importShape)))
          case (ModuleDependency(mname, clss, None, dependencies), None) =>
            acc.updated(mname, (clss, None))
          case _ => throw new Exception(f"Should never be a Typed Module with a typed import or an untyped module with an untyped import: $mod | $shape")
    }
    scopeMap


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
      dependencies.foldLeft(updatedAcc){
        case (acc, (mod, _)) => 
        mod.findReachableModulesHelper(acc)
      }
  
  /**
    * Gets the module dependency if is reachable from the current node in this DAG, 
    * returning it as an Option.
    *
    * @param mnameToFind the module name to find
    * @return Some(ModuleDependency) if exists otherwise None
    */
  def findModuleInDAG(mnameToFind: String): Option[ModuleDependency] =

    @tailrec
    def searchSubTreeForDependency(dependencies: List[(ModuleDependency, Option[CleanShapeType])], visited: Set[String]): Option[ModuleDependency] = {
      dependencies match {
        case Nil => None
        case (mod @ ModuleDependency(mname, clss, shape, dependencies), _) :: tail =>
          if visited.contains(mname) then 
            searchSubTreeForDependency(tail, visited)
          else if (mname == mnameToFind) 
            Some(mod)
          else 
            val updatedVisited = visited + mname
            searchSubTreeForDependency(tail ++ dependencies, updatedVisited)
      }
    }

    searchSubTreeForDependency(this.dependencies, Set.empty)

  /**
    * Generates the Rename Map for the module in this scope by traversing
    * its dependencies in the order imported.
    *
    * @return the map from class names to their renamed Class Name
    */
  def generateRenameMap(): Map[String, String] =
    val renameMap = this.dependencies.foldLeft(Map.empty[String,String]){
      case (acc, (mod, _)) => 
        mod match
          case ModuleDependency(depMname, depClss, _, _) =>
            val (cname, qualifiedName) = generatedQualifiedName(depMname, depClss)
            acc.updated(cname, qualifiedName)
    }
    val (cname, qualifiedName) = generatedQualifiedName(mname, clss)
    renameMap.updated(cname, qualifiedName)

  private def generatedQualifiedName(mname: String, clss: CleanClass): (String, String) =
    (clss.cname, f"$mname.${clss.cname}")

  override def toString: String = 
    val (result, prevShape, visited) = toStringHelper("", "", Set.empty, None)
    result

  private def toStringHelper(
    prefix: String, 
    childPrefix: String, 
    visited: Set[(String, Option[CleanShapeType])],
    importShape: Option[CleanShapeType]
  ): (String, Option[CleanShapeType], Set[(String, Option[CleanShapeType])]) =
    val result = new StringBuilder()
    result.append(prefix).append(mname)
    
    if visited.contains((mname, importShape)) then
      result.append(" (already shown)")
      (result.toString(), importShape, visited)
    else
      val updatedVisited = visited.incl((mname, importShape))
      
      val (finalResult, newShape, finalVisited) =
        dependencies.zipWithIndex.foldLeft((result.toString: String, importShape: Option[CleanShapeType], updatedVisited: Set[(String, Option[CleanShapeType])])) {
        case ((accResult, _, accVisited), ((modDep, newImportShape), index)) =>
          val isLast = index == dependencies.length - 1

          val currentPrefix = newImportShape match
            case Some(value) =>
              if isLast then childPrefix + s"└──[$value] "
              else childPrefix + s"├──[$value] "

            case None =>
              if isLast then childPrefix + "└── "
              else childPrefix + "├── "

          val nextChildPrefix =
            if isLast then childPrefix + "    "
            else childPrefix + "│   "

          val (depString, returnedShape, newVisited) =
            modDep.toStringHelper(currentPrefix, nextChildPrefix, accVisited, newImportShape)

          (accResult + "\n" + depString, returnedShape, newVisited)
      }
    
      (finalResult, newShape, finalVisited)

object ModuleDependency:

  /**
    * Constructs a DAG Module Dependency graph by creating nodes that represent 
    * typed and untyped modules with edges that represent typed and untyped imports
    * to other modules.
    * 
    * Since we are guaranteed by the validity checker that all modules and imports
    * have correct scope, there shouldn't be any cycles or throws 
    * from the importToModule method nor from the modules in scope lookup.
    * 
    * Since we are guaranteed by the import checker that import rules are followed,
    * there shouldn't be any exceptions thrown from import violations.
    * 
    * @param mname the module name of the current module
    * @param clas the class name of the current moduled
    * @param modulesInScopeMap map from module to modules available for import (based on hierachical scope)
    * @param imports the list of imports for the current module
    * @return
    */
  def apply(mname: String, clas: CleanClass, modulesInScopeMap: Map[String, Set[CleanModule]], imports: List[CleanImport]): ModuleDependency =
    buildDAG(mname, clas, None, modulesInScopeMap, imports, MutableMap.empty)

  private def buildDAG(
    mname: String,
    clas: CleanClass,
    shape: Option[CleanShapeType],
    modulesInScopeMap: Map[String, Set[CleanModule]],
    imports: List[CleanImport],
    memoization: MutableMap[String, ModuleDependency]
  ): ModuleDependency =
    
    // Check if we've already built this module's dependency tree
    memoization.get(mname) match
      case Some(existing) => existing
      case None =>
        // Build dependencies first, then memoize to handle potential cycles
        val dependencies = imports.map { imp =>
          (importToModule(modulesInScopeMap(mname), imp), imp) match
            case (Module.Typed(importedMname, importedImports, importedClas, importedShape), Import.Untyped(_)) =>
              (
                buildDAG(importedMname, importedClas, Some(importedShape), modulesInScopeMap, importedImports, memoization),
                None
              )
            case (Module.Untyped(importedMname, importedImports, importedClas), Import.Typed(_, importedShape)) =>
              (
                buildDAG(importedMname, importedClas, None, modulesInScopeMap, importedImports, memoization),
                Some(importedShape)
              )
            case (Module.Untyped(importedMname, importedImports, importedClas), Import.Untyped(_)) =>
              (
                buildDAG(importedMname, importedClas, None, modulesInScopeMap, importedImports, memoization),
                None
              )
            case (mod @ _, _) => throw new Exception(f"Should never be a Typed Module with a typed import or an untyped module with an untyped import: $imp | $mod")
        }
        
        val result = ModuleDependency(mname, clas, shape, dependencies)
        memoization.update(mname, result)
        result

  def importToModule(modules: Set[CleanModule], imp: CleanImport): CleanModule =
    modules.find(module => module.moduleName == imp.importedModName) match
      case None => throw new Exception(f"Invalid import $imp in $modules")
      case Some(module) => module 
