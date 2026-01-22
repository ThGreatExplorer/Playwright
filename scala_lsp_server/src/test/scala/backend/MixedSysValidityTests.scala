package test.backend

import munit.FunSuite
import ast._
import test.backend.AstRangeDefaults.DummyRange
import ast.ValidityErrNodes._
import ast.ConverterToClean._
import static._

class VCheckImportsTest extends FunSuite:

    def makeScopedMap(expectedMap : Map[String, Option[CleanShapeType]]) : ScopedModuleData =
        val entries = expectedMap.toList.map((mname, shape) => 
            ModuleDataEntry(Module[Clean](mname, Nil, Class[Clean]("dummy", Nil, Nil, shape, DummyRange), DummyRange)))
        ScopedModuleData(entries)

    test("checkImportsModules - mixed mods, no imports") {
        val shape : CleanShapeType = Type.Shape(List(FieldType[Clean]("x", Type.Number(DummyRange), DummyRange)), Nil, DummyRange)
        val modules = 
            List(
                Module[Clean]("Untyped1", Nil, Class[Clean]("C1", Nil, Nil, None, DummyRange), DummyRange),
                Module[Clean]("Typed1", Nil, Class[Clean]("C2", Nil, Nil, Some(shape), DummyRange), DummyRange),
                Module[Clean]("Untyped2", Nil, Class[Clean]("C3", Nil, Nil, None, DummyRange), DummyRange)
            )
            
        val mdata = ModuleData(modules)
        val processed = VCheckImports.checkImportsModules(modules, mdata)
        
        processed.map(moduleWEToClean(_).isDefined).foreach(assert(_))
    }

    test("checkMixedImports - imports of undefined modules don't produce errors") {
        val imports = List(
            Import.Untyped[Clean]("UndefinedMod1", DummyRange),
            Import.Typed[Clean]("UndefinedMod2", Type.Shape(Nil, Nil, DummyRange), DummyRange)
        )
        
        val mdata = ModuleData(Nil)
        val processed = VCheckImports.checkMixedImports(imports, mdata.atTopLevel)

        processed.map(importToClean(_).isDefined).foreach(assert(_))
    }

    test("checkMixedImports - untyped import of untyped module from typed context errors") {
        val imports = List(Import.Untyped[Clean]("UntypedMod", DummyRange))
        val modMap:  ScopedModuleData = makeScopedMap(Map("UntypedMod" -> None))

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        assertEquals(processed.length, 1)
        processed.map(importToClean(_).isEmpty).foreach(assert(_))
        processed.head match
            case WE.Err(UntypedModImportedWithoutTImport) => ()
            case _ => fail("Expected UntypedModImportedWithoutTImport error")
    }

    test("checkMixedImports - untyped import of typed module succeeds") {
        val imports = List(Import.Untyped[Clean]("TypedMod", DummyRange))
        val modMap:  ScopedModuleData = makeScopedMap(Map("TypedMod" -> Some(Type.Shape(Nil, Nil, DummyRange))))

        val result = VCheckImports.checkMixedImports(imports, modMap)

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        assertEquals(processed.length, 1)
        processed.map(importToClean(_).isDefined).foreach(assert(_))
    }

    test("checkMixedImports - typed import of typed module errors") {
        val shape : CleanShapeType = Type.Shape(Nil, Nil, DummyRange)
        val imports = List(Import.Typed[Clean]("TypedMod", shape, DummyRange))
        val modMap:  ScopedModuleData = makeScopedMap(Map("TypedMod" -> Some(shape)))

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        assertEquals(processed.length, 1)
        processed.map(importToClean(_).isEmpty).foreach(assert(_))
        processed.head match
            case WE.Err(TypedModTImported) => ()
            case _ => fail("Expected TypedModTImported error")
    }

    test("checkMixedImports - typed import of untyped module succeeds") {
        val shape : CleanShapeType = Type.Shape(Nil, Nil, DummyRange)
        val imports = List(Import.Typed[Clean]("UntypedMod", shape, DummyRange))
        val modMap:  ScopedModuleData = makeScopedMap(Map("UntypedMod" -> None))

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        assertEquals(processed.length, 1)
        processed.map(importToClean(_).isDefined).foreach(assert(_))
    }

    test("checkMixedImports - typed import of same untyped module with same shape twice succeeds") {
        val shape : CleanShapeType =
            Type.Shape(List(FieldType[Clean]("x", Type.Number[Clean](DummyRange), DummyRange)), Nil, DummyRange)
        val imports = List(
            Import.Typed[Clean]("UntypedMod", shape, DummyRange),
            Import.Untyped[Clean]("TypedMod", DummyRange),
            Import.Typed[Clean]("UntypedMod", shape, DummyRange)
        )
        val modMap:  ScopedModuleData = makeScopedMap(Map("UntypedMod" -> None, "TypedMod" -> Some(shape)))

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        assertEquals(processed.length, 3)
        processed.map(importToClean(_).isDefined).foreach(assert(_))
    }

    test("checkMixedImports - typed import of same untyped module with different shapes errors") {
        val shape1 : CleanShapeType =
            Type.Shape(List(FieldType[Clean]("x", Type.Number[Clean](DummyRange), DummyRange)), Nil, DummyRange)
        val shape2 : CleanShapeType =
            Type.Shape(List(FieldType[Clean]("y", Type.Number[Clean](DummyRange), DummyRange)), Nil, DummyRange)
        val imports = List(
            Import.Typed[Clean]("UntypedMod", shape1, DummyRange),
            Import.Typed[Clean]("UntypedMod", shape2, DummyRange)
        )
        val modMap:  ScopedModuleData = makeScopedMap(Map("UntypedMod" -> None))

        val processed = VCheckImports.checkMixedImports(imports, modMap)

        processed match
            case WE.Node(_) :: WE.Err(UntypedModTImportedWithDiffShape) :: Nil => ()
            case _ => fail("Expected UntypedModTImportedWithDiffShape error")
    }

    test("checkMixedImports - multiple imports with mixed validity") {
        val shape1 : CleanShapeType =
            Type.Shape(List(FieldType[Clean]("a", Type.Number[Clean](DummyRange), DummyRange)), Nil, DummyRange)
        val shape2 : CleanShapeType =
            Type.Shape(List(FieldType[Clean]("b", Type.Number[Clean](DummyRange), DummyRange)), Nil, DummyRange)
        
        val imports = List(
            Import.Untyped[Clean]("TypedMod", DummyRange),           // OK - untyped import of typed
            Import.Typed[Clean]("UntypedMod1", shape1, DummyRange),  // OK - first typed import
            Import.Typed[Clean]("UntypedMod1", shape1, DummyRange),  // OK - same shape
            Import.Typed[Clean]("UntypedMod1", shape2, DummyRange),  // ERROR - different shape
            Import.Untyped[Clean]("UntypedMod2", DummyRange)         // ERROR - untyped of untyped
        )
        
        val modMap:  ScopedModuleData = makeScopedMap(Map(
            "TypedMod" -> Some(Type.Shape(Nil, Nil, DummyRange)),
            "UntypedMod1" -> None,
            "UntypedMod2" -> None
        ))

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        processed match
            case WE.Node(_) :: WE.Node(_) :: WE.Node(_) 
                 :: WE.Err(UntypedModTImportedWithDiffShape) 
                 :: WE.Err(UntypedModImportedWithoutTImport) :: Nil => ()
            case _ => fail("Unexpected return: " + processed)
    }

    test("checkImportsSys - end-to-end system validation (does not flag undefined imports)") {
        val shape : CleanShapeType =
            Type.Shape(List(FieldType[Clean]("field", Type.Number[Clean](DummyRange), DummyRange)), Nil, DummyRange)
        val modules : List[CleanModule] = List(
                Module(
                    "ModA",
                    List(Import.Untyped("ModB", DummyRange)),
                    Class("ClassA", Nil, Nil, Some(Type.Shape(Nil, Nil, DummyRange)), DummyRange),
                    DummyRange
                ),
                Module(
                    "ModB",
                    Nil,
                    Class("ClassB", Nil, Nil, None, DummyRange),
                    DummyRange
                )
            )
        val system = System[Clean](
            modules,
            List(Import.Untyped("ModA", DummyRange)),
            ProgBlock(Nil, Nil, Expr.Num(0.0, DummyRange), DummyRange),
            ModuleData(modules),
            DummyRange
        )

        val result = VCheckImports.checkImportsSys(system)
        assert(systemToClean(result).isDefined)
    }

    test("checkImportsSys - detects import errors in nested modules") {
        val modules : List[CleanModule] = List(
                Module(
                    "UntypedMod",
                    Nil,
                    Class("ClassB", Nil, Nil, None, DummyRange),
                    DummyRange,
                ),
                Module[Clean](
                    "TypedMod",
                    List(Import.Untyped("UntypedMod", DummyRange)), // This should error
                    Class[Clean]("ClassA", Nil, Nil, Some(Type.Shape[Clean](Nil, Nil, DummyRange)), DummyRange),
                    DummyRange,
                )                
            )
        val system = System[Clean](
            modules,
            Nil,
            ProgBlock(Nil, Nil, Expr.Num(0.0, DummyRange), DummyRange),
            ModuleData(modules),
            DummyRange
        )

        val result = VCheckImports.checkImportsSys(system)
        assert(systemToClean(result).isEmpty)

        result match
            case WE.Node(sys) =>
                // Second module should have import error
                sys.modules(1) match
                    case WE.Node(Module(_, imports, _, _)) =>
                        imports.head match
                            case WE.Err(UntypedModImportedWithoutTImport) => ()
                            case _ => fail("Expected import error in first module")
                    case _ => fail("Expected typed module")
            case _ => fail("Expected system node")
    }
