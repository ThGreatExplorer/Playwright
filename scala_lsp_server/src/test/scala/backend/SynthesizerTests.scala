package test.backend

import munit.FunSuite
import ast._
import test.backend.AstRangeDefaults.DummyRange
import static.ModuleData
import linker._
 import util.getMDNames

class SynthesizerTest extends FunSuite:

    test("synthesizeImports - typed import of untyped module creates new typed module") {
        val shape : CleanShapeType =
            Type.Shape(List(FieldType("x", Type.Number(DummyRange), DummyRange)), Nil, DummyRange)
        val modules = List(
            Module[Clean]("ModA", Nil, Class("ClassA", Nil, Nil, None, DummyRange), DummyRange)
        )
        val moduleData = ModuleData(modules)
        
        val imports = List(Import.Typed[Clean]("ModA", shape, DummyRange))
        val (newMods, updImports) = Synthesizer.synthesizeImports(imports, "ModB", moduleData)
        
        assertEquals(newMods.length, 1)
        assertEquals(updImports.length, 1)
        
        newMods.head match
            case Module(mname, imps, clas @ Class(cname, fields, methods, Some(s), _), _) =>
                assertEquals(mname, "ModA.into.ModB")
                assertEquals(s, shape)
                clas match
                    case Class(cname, _, _, _, _) => assertEquals(cname, "ClassA")
            case _ => fail("Expected typed module")
        
        updImports.head match
            case Import.Untyped(mname, _) => assertEquals(mname, "ModA.into.ModB")
            case _ => fail("Expected untyped import")
    }

    test("synthesizeImports - mixed typed and untyped imports") {
        val shape : CleanShapeType =
            Type.Shape(List(FieldType("x", Type.Number(DummyRange), DummyRange)), Nil, DummyRange)
        val modules = List(
            Module[Clean]("ModA", Nil, Class("ClassA", Nil, Nil, None, DummyRange), DummyRange),
            Module[Clean]("ModB", Nil, Class("ClassB", Nil, Nil, Some(shape), DummyRange), DummyRange)
        )
        val moduleData = ModuleData(modules)
        
        val imports = List(
            Import.Typed[Clean]("ModA", shape, DummyRange),
            Import.Untyped[Clean]("ModB", DummyRange)
        )
        val (newMods, updImports) = Synthesizer.synthesizeImports(imports, "ModC", moduleData)
        
        assertEquals(newMods.length, 1)
        assertEquals(updImports.length, 2)
        
        newMods.head match
            case m @ Module(mname, _, _, _) if m.isTyped => assertEquals(mname, "ModA.into.ModC")
            case _ => fail("Expected typed module")
        
        updImports(0) match
            case Import.Untyped(mname, _) => assertEquals(mname, "ModA.into.ModC")
            case _ => fail("Expected untyped import")
        
        updImports(1) match
            case Import.Untyped(mname, _) => assertEquals(mname, "ModB")
            case _ => fail("Expected untyped import")
    }

    test("synthesizeImports - mixed typed and untyped imports, only distinct typed imports produce clones") {
        val shape : CleanShapeType =
            Type.Shape(List(FieldType("x", Type.Number(DummyRange), DummyRange)), Nil, DummyRange)
        val modules = List(
            Module[Clean]("ModA", Nil, Class("ClassA", Nil, Nil, None, DummyRange), DummyRange),
            Module[Clean]("ModB", Nil, Class("ClassB", Nil, Nil, Some(shape), DummyRange), DummyRange)
        )
        val moduleData = ModuleData(modules)
        
        val imports = List(
            Import.Typed[Clean]("ModA", shape, DummyRange),
            Import.Untyped[Clean]("ModB", DummyRange),
            Import.Typed[Clean]("ModA", shape, DummyRange),
            Import.Untyped[Clean]("ModB", DummyRange),
            Import.Typed[Clean]("ModA", shape, DummyRange),
            Import.Untyped[Clean]("ModB", DummyRange)
        )
        val (newMods, updImports) = Synthesizer.synthesizeImports(imports, "ModC", moduleData)
        
        assertEquals(newMods.length, 1)
        assertEquals(updImports.length, 6)
        
        newMods.head match
            case m @ Module(mname, _, _, _) if m.isTyped => assertEquals(mname, "ModA.into.ModC")
            case _ => fail("Expected typed module")
        
        updImports(0) match
            case Import.Untyped(mname, _) => assertEquals(mname, "ModA.into.ModC")
            case _ => fail("Expected untyped import")
        
        updImports(1) match
            case Import.Untyped(mname, _) => assertEquals(mname, "ModB")
            case _ => fail("Expected untyped import")
    }

    test("synthesizeImports - preserves imports from original untyped module") {
        val shape : CleanShapeType = Type.Shape(Nil, Nil, DummyRange)
        val origImports : List[CleanUntypedImport] = List(Import.Untyped[Clean]("DepMod", DummyRange))
        val modules = List(
            Module[Clean]("ModA", origImports, Class("ClassA", Nil, Nil, None, DummyRange), DummyRange),
            Module[Clean]("DepMod", Nil, Class("DepClass", Nil, Nil, None, DummyRange), DummyRange)
        )
        val moduleData = ModuleData(modules)
        
        val imports = List(Import.Typed[Clean]("ModA", shape, DummyRange))
        val (newMods, _) = Synthesizer.synthesizeImports(imports, "ModB", moduleData)
        
        assertEquals(newMods.length, 1)
        newMods.head match
            case m @ Module(_, imps, _, _) if m.isTyped =>
                assertEquals(imps.length, 1)
                assertEquals(imps, origImports)
            case _ => fail("Expected typed module")
    }

    test("synthesizeModules - untyped modules pass through unchanged") {
        val modules = List(
            Module[Clean]("ModA", Nil, Class("ClassA", Nil, Nil, None, DummyRange), DummyRange),
            Module[Clean]("ModB", Nil, Class("ClassB", Nil, Nil, None, DummyRange), DummyRange)
        )
        val moduleData = ModuleData(modules)
        
        val result = Synthesizer.synthesizeModules(modules, moduleData)
        
        assertEquals(result.length, 2)
        result(0) match
            case Module(mname, _, _, _) => assertEquals(mname, "ModA")
        result(1) match
            case Module(mname, _, _, _) => assertEquals(mname, "ModB")
    }

    test("synthesizeModules - typed module with typed import creates new module") {
        val shape : CleanShapeType =
            Type.Shape(List(FieldType("x", Type.Number(DummyRange), DummyRange)), Nil, DummyRange)
        val modules = List(
            Module[Clean]("ModA", Nil, Class("ClassA", Nil, Nil, None, DummyRange), DummyRange),
            Module[Clean](
                "ModB",
                List(Import.Typed[Clean]("ModA", shape, DummyRange)),
                Class("ClassB", Nil, Nil, Some(shape), DummyRange),
                DummyRange
            )
        )
        val moduleData = ModuleData(modules)
        
        val result = Synthesizer.synthesizeModules(modules, moduleData)
        
        assertEquals(result.length, 3)
        
        result(0) match
            case Module(mname, _, _, _) => assertEquals(mname, "ModA")
        
        result(1) match
            case m @ Module(mname, _, _, _) if m.isTyped => assertEquals(mname, "ModA.into.ModB")
            case _ => fail("Expected synthesized typed module")
        
        result(2) match
            case m @ Module(mname, imps, _, _) if m.isTyped =>
                assertEquals(mname, "ModB")
                assertEquals(imps.length, 1)
                imps.head match
                    case Import.Untyped(importName, _) => assertEquals(importName, "ModA.into.ModB")
                    case _ => fail("Expected untyped import")
            case _ => fail("Expected typed module")
    }

    test("synthesizeModules - multiple typed modules with typed imports") {
        val shape1 : CleanShapeType =
            Type.Shape(List(FieldType("x", Type.Number(DummyRange), DummyRange)), Nil, DummyRange)
        val shape2 : CleanShapeType =
            Type.Shape(List(FieldType("y", Type.Number(DummyRange), DummyRange)), Nil, DummyRange)
        val modules = List(
            Module[Clean]("ModA", Nil, Class("ClassA", Nil, Nil, None, DummyRange), DummyRange),
            Module[Clean](
                "ModB",
                List(Import.Typed[Clean]("ModA", shape1, DummyRange)),
                Class("ClassB", Nil, Nil, Some(shape1), DummyRange),
                DummyRange
            ),
            Module[Clean](
                "ModC",
                List(Import.Typed[Clean]("ModA", shape2, DummyRange)),
                Class("ClassC", Nil, Nil, Some(shape2), DummyRange),
                DummyRange
            )
        )
        val moduleData = ModuleData(modules)
        val result = Synthesizer.synthesizeModules(modules, moduleData)
        
        assertEquals(result.length, 5)
        
        val moduleNames = result.getMDNames
        
        assert(moduleNames.contains("ModA"))
        assert(moduleNames.contains("ModA.into.ModB"))
        assert(moduleNames.contains("ModB"))
        assert(moduleNames.contains("ModA.into.ModC"))
        assert(moduleNames.contains("ModC"))
    }

    test("synthesizeSystem - end-to-end with typed import in system body") {
        val shape : CleanShapeType =
            Type.Shape(List(FieldType("x", Type.Number(DummyRange), DummyRange)), Nil, DummyRange)
        val modules = List(
                Module[Clean]("ModA", Nil, Class("ClassA", Nil, Nil, None, DummyRange), DummyRange)
            )
        val system = System[Clean](
            modules,
            List(Import.Typed[Clean]("ModA", shape, DummyRange)),
            ProgBlock(Nil, Nil, Expr.Num(0.0, DummyRange), DummyRange),
            ModuleData(modules),
            DummyRange
        )
        
        val result = Synthesizer.synthesizeSystem(system)
        
        result match
            case System(mods, imps, _, _, _) =>
                assertEquals(mods.length, 2)
                assertEquals(imps.length, 1)
                
                mods(1) match
                    case m @ Module(mname, _, _, _) if m.isTyped => assertEquals(mname, "ModA.into.Body")
                    case _ => fail("Expected synthesized typed module")
                
                imps.head match
                    case Import.Untyped(mname, _) => assertEquals(mname, "ModA.into.Body")
                    case _ => fail("Expected untyped import")
    }

    test("synthesizeSystem - complex nested typed imports") {
        val shape1 : CleanShapeType =
            Type.Shape(List(FieldType("x", Type.Number(DummyRange), DummyRange)), Nil, DummyRange)
        val shape2 : CleanShapeType =
            Type.Shape(List(FieldType("y", Type.Number(DummyRange), DummyRange)), Nil, DummyRange)
        val modules = List(
                Module[Clean]("ModA", Nil, Class("ClassA", Nil, Nil, None, DummyRange), DummyRange),
                Module[Clean](
                    "ModB", 
                    List(Import.Typed[Clean]("ModA", shape1, DummyRange)), 
                    Class("ClassB", Nil, Nil, Some(shape1), DummyRange),
                    DummyRange
                )
            )
        val system = System[Clean](
            modules,
            List(
                Import.Typed[Clean]("ModA", shape2, DummyRange),
                Import.Untyped[Clean]("ModB", DummyRange)
            ),
            ProgBlock(Nil, Nil, Expr.Num(0.0, DummyRange), DummyRange),
            ModuleData(modules),
            DummyRange
        )
        
        val result = Synthesizer.synthesizeSystem(system)
        
        result match
            case System(mods, imps, _, _, _) =>
                assertEquals(mods.length, 4)
                assertEquals(imps.length, 2)
                
                val moduleNames = mods.getMDNames
                
                assert(moduleNames.contains("ModA"))
                assert(moduleNames.contains("ModA.into.ModB"))
                assert(moduleNames.contains("ModB"))
                assert(moduleNames.contains("ModA.into.Body"))

                mods(1) match
                    case Module(mname, _, Class(cname, fields, methods, Some(shape), _), _) => 
                        assertEquals(mname, "ModA.into.ModB")
                        assertEquals(shape, shape1)
                    case _ => fail("Expected synthesized typed module")
                
                mods(3) match
                    case Module(mname, _, Class(cname, fields, methods, Some(shape), _), _) => 
                        assertEquals(mname, "ModA.into.Body")
                        assertEquals(shape, shape2)
                    case _ => fail("Expected synthesized typed module")

    }
