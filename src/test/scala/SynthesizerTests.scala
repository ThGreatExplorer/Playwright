package test

import munit.FunSuite
import ast._
import static.ModuleData
import linker._

class SynthesizerTest extends FunSuite:

    test("synthesizeImports - typed import of untyped module creates new typed module") {
        val shape : CleanShapeType = Type.Shape(List(FieldType("x", Type.Number())), Nil)
        val modules = List(
            Module.Untyped[Clean]("ModA", Nil, Class("ClassA", Nil, Nil))
        )
        val moduleData = ModuleData(modules)
        
        val imports = List(Import.Typed[Clean]("ModA", shape))
        val (newMods, updImports) = Synthesizer.synthesizeImports(imports, "ModB", moduleData)
        
        assertEquals(newMods.length, 1)
        assertEquals(updImports.length, 1)
        
        newMods.head match
            case Module.Typed(mname, imps, clas, s) =>
                assertEquals(mname, "ModA.into.ModB")
                assertEquals(s, shape)
                clas match
                    case Class(cname, _, _) => assertEquals(cname, "ClassA")
            case _ => fail("Expected typed module")
        
        updImports.head match
            case Import.Untyped(mname) => assertEquals(mname, "ModA.into.ModB")
            case _ => fail("Expected untyped import")
    }

    test("synthesizeImports - mixed typed and untyped imports") {
        val shape : CleanShapeType = Type.Shape(List(FieldType("x", Type.Number())), Nil)
        val modules = List(
            Module.Untyped[Clean]("ModA", Nil, Class("ClassA", Nil, Nil)),
            Module.Typed[Clean]("ModB", Nil, Class("ClassB", Nil, Nil), shape)
        )
        val moduleData = ModuleData(modules)
        
        val imports = List(
            Import.Typed[Clean]("ModA", shape),
            Import.Untyped[Clean]("ModB")
        )
        val (newMods, updImports) = Synthesizer.synthesizeImports(imports, "ModC", moduleData)
        
        assertEquals(newMods.length, 1)
        assertEquals(updImports.length, 2)
        
        newMods.head match
            case Module.Typed(mname, _, _, _) => assertEquals(mname, "ModA.into.ModC")
            case _ => fail("Expected typed module")
        
        updImports(0) match
            case Import.Untyped(mname) => assertEquals(mname, "ModA.into.ModC")
            case _ => fail("Expected untyped import")
        
        updImports(1) match
            case Import.Untyped(mname) => assertEquals(mname, "ModB")
            case _ => fail("Expected untyped import")
    }

    test("synthesizeImports - mixed typed and untyped imports, only distinct typed imports produce clones") {
        val shape : CleanShapeType = Type.Shape(List(FieldType("x", Type.Number())), Nil)
        val modules = List(
            Module.Untyped[Clean]("ModA", Nil, Class("ClassA", Nil, Nil)),
            Module.Typed[Clean]("ModB", Nil, Class("ClassB", Nil, Nil), shape)
        )
        val moduleData = ModuleData(modules)
        
        val imports = List(
            Import.Typed[Clean]("ModA", shape),
            Import.Untyped[Clean]("ModB"),
            Import.Typed[Clean]("ModA", shape),
            Import.Untyped[Clean]("ModB"),
            Import.Typed[Clean]("ModA", shape),
            Import.Untyped[Clean]("ModB")
        )
        val (newMods, updImports) = Synthesizer.synthesizeImports(imports, "ModC", moduleData)
        
        assertEquals(newMods.length, 1)
        assertEquals(updImports.length, 6)
        
        newMods.head match
            case Module.Typed(mname, _, _, _) => assertEquals(mname, "ModA.into.ModC")
            case _ => fail("Expected typed module")
        
        updImports(0) match
            case Import.Untyped(mname) => assertEquals(mname, "ModA.into.ModC")
            case _ => fail("Expected untyped import")
        
        updImports(1) match
            case Import.Untyped(mname) => assertEquals(mname, "ModB")
            case _ => fail("Expected untyped import")
    }

    test("synthesizeImports - preserves imports from original untyped module") {
        val shape : CleanShapeType = Type.Shape(Nil, Nil)
        val origImports : List[CleanUntypedImport] = List(Import.Untyped[Clean]("DepMod"))
        val modules = List(
            Module.Untyped[Clean]("ModA", origImports, Class("ClassA", Nil, Nil)),
            Module.Untyped[Clean]("DepMod", Nil, Class("DepClass", Nil, Nil))
        )
        val moduleData = ModuleData(modules)
        
        val imports = List(Import.Typed[Clean]("ModA", shape))
        val (newMods, _) = Synthesizer.synthesizeImports(imports, "ModB", moduleData)
        
        assertEquals(newMods.length, 1)
        newMods.head match
            case Module.Typed(_, imps, _, _) =>
                assertEquals(imps.length, 1)
                assertEquals(imps, origImports)
            case _ => fail("Expected typed module")
    }

    test("synthesizeModules - untyped modules pass through unchanged") {
        val modules = List(
            Module.Untyped[Clean]("ModA", Nil, Class("ClassA", Nil, Nil)),
            Module.Untyped[Clean]("ModB", Nil, Class("ClassB", Nil, Nil))
        )
        val moduleData = ModuleData(modules)
        
        val result = Synthesizer.synthesizeModules(modules, moduleData)
        
        assertEquals(result.length, 2)
        result(0) match
            case Module.Untyped(mname, _, _) => assertEquals(mname, "ModA")
            case _ => fail("Expected untyped module")
        result(1) match
            case Module.Untyped(mname, _, _) => assertEquals(mname, "ModB")
            case _ => fail("Expected untyped module")
    }

    test("synthesizeModules - typed module with typed import creates new module") {
        val shape : CleanShapeType = Type.Shape(List(FieldType("x", Type.Number())), Nil)
        val modules = List(
            Module.Untyped[Clean]("ModA", Nil, Class("ClassA", Nil, Nil)),
            Module.Typed[Clean]("ModB", List(Import.Typed[Clean]("ModA", shape)), Class("ClassB", Nil, Nil), shape)
        )
        val moduleData = ModuleData(modules)
        
        val result = Synthesizer.synthesizeModules(modules, moduleData)
        
        assertEquals(result.length, 3)
        
        result(0) match
            case Module.Untyped(mname, _, _) => assertEquals(mname, "ModA")
            case _ => fail("Expected untyped module")
        
        result(1) match
            case Module.Typed(mname, _, _, _) => assertEquals(mname, "ModA.into.ModB")
            case _ => fail("Expected synthesized typed module")
        
        result(2) match
            case Module.Typed(mname, imps, _, _) =>
                assertEquals(mname, "ModB")
                assertEquals(imps.length, 1)
                imps.head match
                    case Import.Untyped(importName) => assertEquals(importName, "ModA.into.ModB")
                    case _ => fail("Expected untyped import")
            case _ => fail("Expected typed module")
    }

    test("synthesizeModules - multiple typed modules with typed imports") {
        val shape1 : CleanShapeType = Type.Shape(List(FieldType("x", Type.Number())), Nil)
        val shape2 : CleanShapeType = Type.Shape(List(FieldType("y", Type.Number())), Nil)
        val modules = List(
            Module.Untyped[Clean]("ModA", Nil, Class("ClassA", Nil, Nil)),
            Module.Typed[Clean]("ModB", List(Import.Typed[Clean]("ModA", shape1)), Class("ClassB", Nil, Nil), shape1),
            Module.Typed[Clean]("ModC", List(Import.Typed[Clean]("ModA", shape2)), Class("ClassC", Nil, Nil), shape2)
        )
        val moduleData = ModuleData(modules)
        val result = Synthesizer.synthesizeModules(modules, moduleData)
        
        assertEquals(result.length, 5)
        
        val moduleNames = result.map {
            case Module.Untyped(mname, _, _) => mname
            case Module.Typed(mname, _, _, _) => mname
        }
        
        assert(moduleNames.contains("ModA"))
        assert(moduleNames.contains("ModA.into.ModB"))
        assert(moduleNames.contains("ModB"))
        assert(moduleNames.contains("ModA.into.ModC"))
        assert(moduleNames.contains("ModC"))
    }

    test("synthesizeSystem - end-to-end with typed import in system body") {
        val shape : CleanShapeType = Type.Shape(List(FieldType("x", Type.Number())), Nil)
        val modules = List(
                Module.Untyped[Clean]("ModA", Nil, Class("ClassA", Nil, Nil))
            )
        val system = System[Clean](
            modules,
            List(Import.Typed[Clean]("ModA", shape)),
            ProgBlock(Nil, Nil, Expr.Num(0.0)),
            ModuleData(modules)
        )
        
        val result = Synthesizer.synthesizeSystem(system)
        
        result match
            case System(mods, imps, _, _) =>
                assertEquals(mods.length, 2)
                assertEquals(imps.length, 1)
                
                mods(1) match
                    case Module.Typed(mname, _, _, _) => assertEquals(mname, "ModA.into.Body")
                    case _ => fail("Expected synthesized typed module")
                
                imps.head match
                    case Import.Untyped(mname) => assertEquals(mname, "ModA.into.Body")
                    case _ => fail("Expected untyped import")
    }

    test("synthesizeSystem - complex nested typed imports") {
        val shape1 : CleanShapeType = Type.Shape(List(FieldType("x", Type.Number())), Nil)
        val shape2 : CleanShapeType = Type.Shape(List(FieldType("y", Type.Number())), Nil)
        val modules = List(
                Module.Untyped[Clean]("ModA", Nil, Class("ClassA", Nil, Nil)),
                Module.Typed[Clean](
                    "ModB", 
                    List(Import.Typed[Clean]("ModA", shape1)), 
                    Class("ClassB", Nil, Nil), 
                    shape1
                )
            )
        val system = System[Clean](
            modules,
            List(
                Import.Typed[Clean]("ModA", shape2),
                Import.Untyped[Clean]("ModB")
            ),
            ProgBlock(Nil, Nil, Expr.Num(0.0)),
            ModuleData(modules)
        )
        
        val result = Synthesizer.synthesizeSystem(system)
        
        result match
            case System(mods, imps, _, _) =>
                assertEquals(mods.length, 4)
                assertEquals(imps.length, 2)
                
                val moduleNames = mods.map {
                    case Module.Untyped(mname, _, _) => mname
                    case Module.Typed(mname, _, _, _) => mname
                }
                
                assert(moduleNames.contains("ModA"))
                assert(moduleNames.contains("ModA.into.ModB"))
                assert(moduleNames.contains("ModB"))
                assert(moduleNames.contains("ModA.into.Body"))

                mods(1) match
                    case Module.Typed(mname, _, _, shape) => 
                        assertEquals(mname, "ModA.into.ModB")
                        assertEquals(shape, shape1)
                    case _ => fail("Expected synthesized typed module")
                
                mods(3) match
                    case Module.Typed(mname, _, _, shape) => 
                        assertEquals(mname, "ModA.into.Body")
                        assertEquals(shape, shape2)
                    case _ => fail("Expected synthesized typed module")

    }