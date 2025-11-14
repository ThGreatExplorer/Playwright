package static

import munit.FunSuite
import ast._
import ast.ValidityErrNodes._
import ast.ConverterToClean._

class VCheckImportsTest extends FunSuite:

    test("checkImportsModules - mixed mods, no imports") {
        val shape : CleanShapeType = Type.Shape(List(FieldType[Clean]("x", Type.Number())), Nil)
        val modules = 
            List(
                Module.Untyped[Clean]("Untyped1", Nil, Class[Clean]("C1", Nil, Nil)),
                Module.Typed[Clean]("Typed1", Nil, Class[Clean]("C2", Nil, Nil), shape),
                Module.Untyped[Clean]("Untyped2", Nil, Class[Clean]("C3", Nil, Nil))
            )
            
        val (processed, modMap) = VCheckImports.checkImportsModules(modules)
        
        processed.map(moduleWEToClean(_).isDefined).foreach(assert(_))
        assertEquals(modMap("Untyped1"), None)
        assertEquals(modMap("Typed1"),   Some(shape))
        assertEquals(modMap("Untyped2"), None)
    }

    test("checkMixedImports - imports of undefined modules don't produce errors") {
        val modMap: VCheckImports.ModToOptShapeMap = Map.empty
        val imports = List(
            Import.Untyped[Clean]("UndefinedMod1"),
            Import.Typed[Clean]("UndefinedMod2", Type.Shape(Nil, Nil))
        )
        
        val processed = VCheckImports.checkMixedImports(imports, modMap)

        processed.map(importToClean(_).isDefined).foreach(assert(_))
    }

    test("checkMixedImports - untyped import of untyped module from typed context errors") {
        val imports = List(Import.Untyped[Clean]("UntypedMod"))
        val modMap: VCheckImports.ModToOptShapeMap = Map("UntypedMod" -> None)

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        assertEquals(processed.length, 1)
        processed.map(importToClean(_).isEmpty).foreach(assert(_))
        processed.head match
            case WE.Err(UntypedModImportedWithoutTImport) => ()
            case _ => fail("Expected UntypedModImportedWithoutTImport error")
    }

    test("checkMixedImports - untyped import of typed module succeeds") {
        val imports = List(Import.Untyped[Clean]("TypedMod"))
        val modMap: VCheckImports.ModToOptShapeMap = Map("TypedMod" -> Some(Type.Shape(Nil, Nil)))

        val result = VCheckImports.checkMixedImports(imports, modMap)

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        assertEquals(processed.length, 1)
        processed.map(importToClean(_).isDefined).foreach(assert(_))
    }

    test("checkMixedImports - typed import of typed module errors") {
        val shape : CleanShapeType = Type.Shape(Nil, Nil)
        val imports = List(Import.Typed[Clean]("TypedMod", shape))
        val modMap: VCheckImports.ModToOptShapeMap = Map("TypedMod" -> Some(shape))

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        assertEquals(processed.length, 1)
        processed.map(importToClean(_).isEmpty).foreach(assert(_))
        processed.head match
            case WE.Err(TypedModTImported) => ()
            case _ => fail("Expected TypedModTImported error")
    }

    test("checkMixedImports - typed import of untyped module succeeds") {
        val shape : CleanShapeType = Type.Shape(Nil, Nil)
        val imports = List(Import.Typed[Clean]("UntypedMod", shape))
        val modMap: VCheckImports.ModToOptShapeMap = Map("UntypedMod" -> None)

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        assertEquals(processed.length, 1)
        processed.map(importToClean(_).isDefined).foreach(assert(_))
    }

    test("checkMixedImports - typed import of same untyped module with same shape twice succeeds") {
        val shape : CleanShapeType = Type.Shape(List(FieldType("x", Type.Number())), Nil)
        val imports = List(
            Import.Typed[Clean]("UntypedMod", shape),
            Import.Untyped[Clean]("TypedMod"),
            Import.Typed[Clean]("UntypedMod", shape)
        )
        val modMap: VCheckImports.ModToOptShapeMap = Map("UntypedMod" -> None, "TypedMod" -> Some(shape))

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        assertEquals(processed.length, 3)
        processed.map(importToClean(_).isDefined).foreach(assert(_))
    }

    test("checkMixedImports - typed import of same untyped module with different shapes errors") {
        val shape1 : CleanShapeType = Type.Shape(List(FieldType("x", Type.Number())), Nil)
        val shape2 : CleanShapeType = Type.Shape(List(FieldType("y", Type.Number())), Nil)
        val imports = List(
            Import.Typed[Clean]("UntypedMod", shape1),
            Import.Typed[Clean]("UntypedMod", shape2)
        )
        val modMap: VCheckImports.ModToOptShapeMap = Map("UntypedMod" -> None)

        val processed = VCheckImports.checkMixedImports(imports, modMap)

        processed match
            case WE.Node(_) :: WE.Err(UntypedModTImportedWithDiffShape) :: Nil => ()
            case _ => fail("Expected UntypedModTImportedWithDiffShape error")
    }

    test("checkMixedImports - multiple imports with mixed validity") {
        val shape1 : CleanShapeType = Type.Shape(List(FieldType("a", Type.Number())), Nil)
        val shape2 : CleanShapeType = Type.Shape(List(FieldType("b", Type.Number())), Nil)
        
        val imports = List(
            Import.Untyped[Clean]("TypedMod"),           // OK - untyped import of typed
            Import.Typed[Clean]("UntypedMod1", shape1),  // OK - first typed import
            Import.Typed[Clean]("UntypedMod1", shape1),  // OK - same shape
            Import.Typed[Clean]("UntypedMod1", shape2),  // ERROR - different shape
            Import.Untyped[Clean]("UntypedMod2")         // ERROR - untyped of untyped
        )
        
        val modMap: VCheckImports.ModToOptShapeMap = Map(
            "TypedMod" -> Some(Type.Shape(Nil, Nil)),
            "UntypedMod1" -> None,
            "UntypedMod2" -> None
        )

        val processed = VCheckImports.checkMixedImports(imports, modMap)
        processed match
            case WE.Node(_) :: WE.Node(_) :: WE.Node(_) 
                 :: WE.Err(UntypedModTImportedWithDiffShape) 
                 :: WE.Err(UntypedModImportedWithoutTImport) :: Nil => ()
            case _ => fail("Unexpected return: " + processed)
    }

    test("checkImportsSys - end-to-end system validation (does not flag undefined imports)") {
        val shape : CleanShapeType = Type.Shape(List(FieldType("field", Type.Number())), Nil)
        val system = System[Clean](
            modules = List(
                Module.Typed(
                    "ModA",
                    List(Import.Untyped("ModB")),
                    Class("ClassA", Nil, Nil),
                    Type.Shape(Nil, Nil)
                ),
                Module.Untyped(
                    "ModB",
                    Nil,
                    Class("ClassB", Nil, Nil)
                )
            ),
            imports = List(Import.Untyped("ModA")),
            progb = ProgBlock(Nil, Nil, Expr.Num(0.0))
        )

        val result = VCheckImports.checkImportsSys(system)
        assert(systemToClean(result).isDefined)
    }

    test("checkImportsSys - detects import errors in nested modules") {
        val system = System[Clean](
            modules = List(
                Module.Untyped(
                    "UntypedMod",
                    Nil,
                    Class("ClassB", Nil, Nil)
                ),
                Module.Typed(
                    "TypedMod",
                    List(Import.Untyped("UntypedMod")), // This should error
                    Class("ClassA", Nil, Nil),
                    Type.Shape(Nil, Nil)
                )                
            ),
            imports = Nil,
            progb = ProgBlock(Nil, Nil, Expr.Num(0.0))
        )

        val result = VCheckImports.checkImportsSys(system)
        assert(systemToClean(result).isEmpty)

        result match
            case WE.Node(sys) =>
                // Second module should have import error
                sys.modules(1) match
                    case WE.Node(Module.Typed(_, imports, _, _)) =>
                        imports.head match
                            case WE.Err(UntypedModImportedWithoutTImport) => ()
                            case _ => fail("Expected import error in first module")
                    case _ => fail("Expected typed module")
            case _ => fail("Expected system node")
    }
