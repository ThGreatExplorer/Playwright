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
    override def toString(): String

object ModuleData:

    type ScopedModuleMap = Map[String, ModuleDataEntry]

    def apply(sys : CleanSystem): ModuleData = sys match
        case System(modules, _, _) => 
            val modDataEntries = modules.map(ModuleDataEntry(_)).toMap 
            new MapModDefs(modDataEntries)

    private class MapModDefs(underlying: Map[String, ModuleDataEntry]) extends ModuleData:
        override def toString(): String = underlying.toString()

        def lookupModule(moduleName: String): ModuleDataEntry =
            underlying.get(moduleName) match
                case Some(mDef) => mDef
                case None =>   
                    // Technically unnecessary throw but it will help us catch env errors early
                    throw new UnreachablePatternMatch(
                        "Should never happen: moduleName " + moduleName + " not found in ModuleData: " + underlying.toString
                    )