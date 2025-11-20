package static

import ast._
import util.UnreachablePatternMatch

final case class ModuleDataEntry(
    imports: List[CleanImport],
    clas: CleanClass,
    shape: Option[CleanShapeType]
)

object ModuleDataEntry:
    def apply(m : CleanModule) : (String, ModuleDataEntry) = m match
        case Module.Typed(mname, imps, clas, shape) => 
            mname -> ModuleDataEntry(imps, clas, Some(shape))
        case Module.Untyped(mname, imps, clas) =>
            mname -> ModuleDataEntry(imps, clas, None)

trait ModuleData:
    def lookupModule(moduleName: String): ModuleDataEntry
    def lookupModuleShape(moduleName: String): Option[CleanShapeType]
    def contains(moduleName: String): Boolean
    override def toString(): String

object ModuleData:

    type ModuleDataMap = Map[String, ModuleDataEntry]
    type ScopedModuleDataMap = Map[String, ModuleDataMap]

    private val topLevelKey = "#TL#"

    def constructFromSystem(sys : CleanSystem): ModuleData = sys match
        case System(modules, _, _) => apply(modules)

    def apply(modules : List[CleanModule]): ModuleData = 
        val modDataEntries = modules.map(ModuleDataEntry(_))
        val scopedModsDataEntries = modDataEntries.zipWithIndex.map{
            case ((mname, entry), index) =>
                val scopedMap = modDataEntries.take(index).toMap
                mname -> scopedMap
        }
        val scopedModsWithTLDataEntries = 
            (topLevelKey -> modDataEntries.toMap) :: scopedModsDataEntries

        new MapModDefs(scopedModsWithTLDataEntries.toMap)

    private class MapModDefs(underlying: ScopedModuleDataMap) extends ModuleData:
        override def toString(): String = underlying.toString()

        def contains(moduleName: String): Boolean =
            underlying.contains(moduleName)

        def lookupModule(moduleName: String): ModuleDataEntry =
            underlying(topLevelKey).get(moduleName) match
                case Some(moduleData) => moduleData
                case None =>   
                    // Technically unnecessary throw but it will help us catch env errors early
                    throw new UnreachablePatternMatch(
                        "Should never happen: moduleName " + moduleName + " not found in ModuleData: " + underlying.toString
                    )
        
        def lookupModuleShape(moduleName: String): Option[CleanShapeType] =
            underlying(topLevelKey).get(moduleName) match
                case Some(ModuleDataEntry(_, _, shape)) => shape
                case None =>   
                    // Technically unnecessary throw but it will help us catch env errors early
                    throw new UnreachablePatternMatch(
                        "Should never happen: moduleName " + moduleName + " not found in ModuleData: " + underlying.toString
                    )