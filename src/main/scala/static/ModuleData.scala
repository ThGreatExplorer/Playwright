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


trait ScopedModuleData:
    def contains(moduleName: String): Boolean
    def lookupModule(moduleName: String): ModuleDataEntry
    def lookupModuleCName(moduleName: String): String
    def lookupModuleShape(moduleName: String): Option[CleanShapeType]
    def lookupTypedCNameAndShape(moduleName: String): (String, CleanShapeType)
    def lookupUntypedCName(moduleName: String): String
    override def toString(): String

object ScopedModuleData:

    def apply(modDataEntries : List[(String, ModuleDataEntry)]): ScopedModuleData = 
        new MapModDefs(modDataEntries.toMap)

    private class MapModDefs(underlying: Map[String, ModuleDataEntry]) extends ScopedModuleData:
        override def toString(): String = underlying.toString()

        def contains(moduleName: String): Boolean =
            underlying.contains(moduleName)

        /**
         * Should be enforced by a path type dependency (but isn't yet) that this method should not be called until
         * after the check undefined validity pass.
         * 
         * Otherwise, the lookup may throw if this method is called prior (or if a random string is passed)
         * 
         * @param moduleName the module to lookup
         * @return the ModuleDataEntry associated to the moduleName
        */
        def lookupModule(moduleName: String): ModuleDataEntry =
            underlying.get(moduleName) match
                case Some(moduleData) => moduleData
                case None =>   
                    // Technically unnecessary throw but it will help us catch env errors early
                    throw new UnreachablePatternMatch(
                        "Should never happen: moduleName " + moduleName + " not found in ModuleData: " + underlying.toString
                    )
        
        /**
         * Should be enforced by a path type dependency (but isn't yet) that this method should not be called until
         * after the check undefined validity pass.
         * 
         * Otherwise, the lookup may throw if this method is called prior (or if a random string is passed)
         * 
         * @param moduleName the module to lookup
         * @return the cname of the class in the module
        */
        def lookupModuleCName(moduleName: String): String =
            this.lookupModule(moduleName) match
                case ModuleDataEntry(_, Class(cname, _, _), _) => cname

        /**
         * Should be enforced by a path type dependency (but isn't yet) that this method should not be called until
         * after the check undefined validity pass.
         * 
         * Otherwise, the lookup may throw if this method is called prior (or if a random string is passed)
         * 
         * @param moduleName the module to lookup
         * @return the shape associated with the module or None
        */
        def lookupModuleShape(moduleName: String): Option[CleanShapeType] =
            this.lookupModule(moduleName) match
                case ModuleDataEntry(_, _, shape) => shape

        /**
         * Should be enforced by a path type dependency (but isn't yet) that this method should not be called until
         * after the check undefined validity pass.
         * 
         * Otherwise, the lookup may throw if this method is called prior (or if a random string is passed)
         * 
         * Additionally, this method will throw if the name of a non-typed module is passed.
         * 
         * @param moduleName the module to lookup
         * @return the cname and shape of the typed module
        */
        def lookupTypedCNameAndShape(moduleName: String): (String, CleanShapeType) =
            this.lookupModule(moduleName) match
                case ModuleDataEntry(_, Class(cname, _, _), Some(shape)) => (cname, shape)
                case _ =>   
                    // Technically unnecessary throw but it will help us catch env errors early
                    throw new UnreachablePatternMatch(
                        "Should never happen: expected moduleName " + moduleName + " to be typed in ModuleData: " + underlying.toString
                    )

        /**
         * Should be enforced by a path type dependency (but isn't yet) that this method should not be called until
         * after the check undefined validity pass.
         * 
         * Otherwise, the lookup may throw if this method is called prior (or if a random string is passed)
         * 
         * Additionally, this method will throw if the name of a typed module is passed.
         * 
         * @param moduleName the module to lookup
         * @return the cname of the untyped module
        */
        def lookupUntypedCName(moduleName: String): String =
            this.lookupModule(moduleName) match
                case ModuleDataEntry(_, Class(cname, _, _), None) => cname
                case _ =>   
                    // Technically unnecessary throw but it will help us catch env errors early
                    throw new UnreachablePatternMatch(
                        "Should never happen: expected moduleName " + moduleName + " to be untyped in ModuleData: " + underlying.toString
                    )

trait ModuleData:
    def atTopLevel: ScopedModuleData
    def scopedAt(moduleName: String): ScopedModuleData
    def lookupModule(moduleName: String): ModuleDataEntry
    override def toString(): String

object ModuleData:
    
    type ScopedModuleDataMap = Map[String, ScopedModuleData]
    val TLModuleName = "Body"

    def processSystem(sys : CleanRawSystem): CleanSystem = sys match
        case RawSystem(modules, imports, progb) => 
            System(modules, imports, progb, apply(modules))

    def apply(modules : List[CleanModule]): ModuleData = 
        val modDataEntries = modules.map(ModuleDataEntry(_))
        val scopedModsDataEntries = modDataEntries.zipWithIndex.map{
            case ((mname, entry), index) =>
                val scopedEntries = modDataEntries.take(index)
                mname -> ScopedModuleData(scopedEntries)
        }
        val scopedModsWithTLDataEntries = 
            (TLModuleName -> ScopedModuleData(modDataEntries)) :: scopedModsDataEntries

        new MapModDefs(scopedModsWithTLDataEntries.toMap)

    private class MapModDefs(underlying: ScopedModuleDataMap) extends ModuleData:
        override def toString(): String = underlying.toString()

        def atTopLevel: ScopedModuleData = underlying(TLModuleName)

        def scopedAt(moduleName: String): ScopedModuleData =
            underlying.get(moduleName) match
                case Some(scopedData) => scopedData
                case None =>   
                    // Technically unnecessary throw but it will help us catch env errors early
                    throw new UnreachablePatternMatch(
                        "Should never happen: moduleName " + moduleName + " not found in ModuleData: " + underlying.toString
                    )

        def lookupModule(moduleName: String): ModuleDataEntry =
            underlying(TLModuleName).lookupModule(moduleName)