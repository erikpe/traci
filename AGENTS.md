# traci — Project guide for agents and contributors

This repository contains a Java ray tracer with a custom scene description language (called “Traci”). It includes:
- A C-style preprocessor (for includes and macros)
- An ANTLR v3 grammar (lexer, parser, tree-walker)
- An interpreter that constructs a Scene model
- A multi-threaded renderer (with optional GUI preview and PNG output)

This document gives an accurate, code-level overview to help automation agents and humans understand, build, run, and extend the project.

## What this project does

- Reads a scene description file written in Traci language
- Preprocesses it (#include, #define, #if/#elif/#else/#endif)
- Parses it into an AST (ANTLR 3.5.2), then walks the tree to build an interpreter program
- Interprets the program to construct a `Scene` (shapes, materials, lights, camera, skybox)
- Renders the scene to a window and/or to a PNG file using multiple threads

Entry point: `se.ejp.traci.main.Main`.

## Build and run

Prerequisites:
- JDK 8+ (project uses plain Java and Ant)
- Apache Ant available on PATH

Build:
- ant jar
  - Generates parser sources from ANTLR grammars
  - Compiles sources into `build/bin`
  - Packages `traci-0.0.1.jar` with dependencies unpacked and Main-Class set

Run (examples):
- java -jar traci-0.0.1.jar scenes/lego/airplane.traci
- java -jar traci-0.0.1.jar -o renders/airplane.png -w 1280 -h 720 scenes/lego/airplane.traci
- java -jar traci-0.0.1.jar -d --aa-level 1 --threads 0 scenes/lego/airplane.traci

Tests:
- ant test
  - Compiles and runs JUnit 4 tests under `test/`
  - XML results go to `build/test.output`, HTML report to `build/test.report`

Note: A prebuilt `traci-0.0.1.jar` may already exist at repo root.

## Command-line interface

Defined in `src/se/ejp/traci/main/options` using Apache Commons CLI:

Required:
- input-file: exactly one Traci source file is required (first non-option argument)

Common options:
- -w, --width SIZE — output width in pixels (default 800)
- -h, --height SIZE — output height in pixels (default 600)
- -a, --aa-level LEVEL — anti-aliasing level (0 = off, typical 1–3)
- -o, --output FILE — PNG output path (omit to skip saving)
- -d, --display — show live render in a Swing window
- --focal-blur-samples NUM — samples/pixel for focal blur (>0 enables)
- --workblock-width SIZE — tile width (default 16)
- --workblock-height SIZE — tile height (default 16)
- --threads NUM — number of worker threads (0 = auto, default)
- --debug — enable debug logs

Preprocessor options:
- -D NAME=VALUE — define macro(s) passed to preprocessor (repeatable)
- -I DIR — add include search directory (repeatable)
- --preprocessor-output FILE — save preprocessed source to file

## High-level architecture

The pipeline is implemented in four stages:

1) Preprocessor (`lang/preprocessor`)
- `PreprocessorRunner` wraps `org.anarres.cpp.Preprocessor`
- Supports include paths, macros, and line markers; preserves comments
- Emits a flat source string with `#line` markers mapping to original files

2) Parse and AST build (`lang/grammar`, `lang/parser`)
- Grammars: `TraciLexer.g`, `TraciParser.g`, `TraciTreeWalker.g` (ANTLR 3.5.2)
- Ant target `generate-parser` creates Java sources under `lang/parser`
- `ParserRunner`:
  - Builds tokens and parser
  - Collects lexer and parser errors with file/include locations via custom `TraciToken` and `IncludeLocation`
  - Runs the tree walker to construct an `Interpreter` program (`BlockNode`)

3) Interpretation to Scene (`lang/interpreter`)
- `Interpreter.run()` evaluates the `BlockNode` in a root `Context`, filling a `Scene`
- Values are wrapped by `TraciValue` (with runtime type enum) and manipulated through `Entities` which apply block semantics to scene objects
- Built-in functions: `BuiltinFunctions` (rand, randint, print, sin, cos, sqrt, length, dot, cross)
- Control flow: `if/elif/else`, `while`, `for (i in a .. b)`, functions, `return`, `global` variables
- Object construction: `ObjectNode` maps language keywords to factory/static methods for shapes, materials, lights, camera, skybox, transformations, color, and vectors

4) Rendering (`render`, `model`)
- Scene model in `src/se/ejp/traci/model` (shapes, CSG, materials, lights, skybox)
- Renderer divides the image into work blocks (`WorkBlock`) and spawns N threads
- Determinism: per-block RNG with deterministic seeds; builtins also use seeded RNG
- Output via `DrawArea` abstraction:
  - GUI: `DynamicJPanelDrawArea` in `MainWindow` (Swing)
  - PNG: `PngDrawArea`

## Source layout (key packages)

- `se.ejp.traci.main`
  - `Main` — entry point; wires preprocessing, parsing, interpretation, and rendering
  - `options` — CLI parsing (`Options`, `Settings` and typed option classes)

- `se.ejp.traci.lang`
  - `grammar` — ANTLR grammars for lexer, parser, tree walker
  - `parser` — generated sources and helpers (`ParserRunner`, `ParseError`, `TraciToken`, `IncludeLocation`)
  - `preprocessor` — C-preprocessor integration (`PreprocessorRunner`)
  - `interpreter` — execution pipeline (`Interpreter`, value system, builtins, AST nodes)

- `se.ejp.traci.model`
  - Scene graph: `Scene`, `Camera`, `Skybox`, `Color`
  - Shapes: `shape/primitive/*` (box, sphere, cylinder, torus, cone, plane, mesh) and `shape/csg/*` (union, difference, intersection)
  - Materials: `material/*` (texture, finish, interior, pigments incl. solid, checker, image)
  - Lights: point and ambient

- `se.ejp.traci.render`
  - `Renderer`, `RenderingThread`, `Raytrace`, `Ray`, `ProgressReporter`

- `se.ejp.traci.math`
  - `Vector`, `Matrix`, `Transformations`, `Transformation`, solvers/utilities

- `se.ejp.traci.gui`
  - GUI draw areas and window plumbing

- `scenes/` — example scenes (e.g., `lego/airplane.traci`)
- `testcode/` — small language samples used by tests
- `test/` — JUnit tests for parser/interpreter/math/renderer/model
- `lib/` — vendored third-party jars used by Ant

## The Traci language (quick reference)

Lexical elements (from `TraciLexer.g`):
- Literals: INT, FLOAT, QSTRING (double-quoted, with escapes), vectors `[x, y, z]`, colors `color [r, g, b]` or `color [r, g, b, a]`
- Operators: `+ - * / ! < <= > >= == != = , ; ..`
- Delimiters: `() [] {}`
- Keywords: `def return global while for in if elif else bbox`
- Object keywords:
  - Primitive shapes: `box cylinder plane sphere torus cone mesh`
  - CSG: `union difference intersection`
  - Transformations: `identity translate scale scalex scaley scalez rotx roty rotz rotAround rotVecToVec`
  - Rendering entities: `texture pigment finish interior material camera skybox`
  - Lights: `pointlight ambientlight`

Grammar highlights (from `TraciParser.g`):
- Functions: `def foo(a, b) { ... }`
- Statements: expression `;`, assignments `x = expr;`, `return expr;`, `global ID = expr;`
- Control flow: `if (cond) { ... } elif (...) { ... } else { ... }`, `while (cond) { ... }`, `for (i in a .. b) { ... }`
- Calls: `foo(1, 2) { ... }` (optional block applies to callee entity)
- Object literals: e.g., `sphere(1.0) { translate [0,1,0]; material { ... } }`
- Vector: `[x, y, z]`
- Color: `color [r, g, b]` or `color [r, g, b, a]`
- Simplified transformation statement: e.g., `translate [1, 0, 0];` or `roty PI/2;` without parentheses is allowed in statement context

Semantics (from `lang/interpreter`):
- Expressions evaluate to `TraciValue` of a specific type (number, boolean, vector, string, shape, material, etc.)
- Attaching blocks: when an object or call is followed by a `{ ... }` block, the block runs “in the context” of that entity; `Entities` maps values applied in the block to operations (e.g., applying a `Transformation` to a `Shape`, setting a `Pigment` on a `Texture`, adding `Light` to `Scene`)
- Builtins: `print(x)`, `rand()`, `randint(start, end)`, `sin`, `cos`, `sqrt`, vector `length`, `dot(a,b)`, `cross(a,b)`

Preprocessing (from `PreprocessorRunner`):
- `#include "file.traci"`, `#define NAME VALUE`, conditional blocks with `#if/#elif/#else/#endif`
- Use `-D` and `-I` options to feed macros and include paths from CLI

## Rendering features

- Multi-threaded: work queue of tiles (default 16×16) distributed across N threads
- Anti-aliasing: grid-based sampling controlled by `--aa-level`
- Focal blur: controlled by `--focal-blur-samples`
- Deterministic random sources: seeded RNG per tile and in builtins to make parallel order independent
- Outputs:
  - GUI preview window (`--display`): live updates while rendering
  - PNG file (`-o path.png`): written at the end

## Example: minimal scene

```
// Save as example.traci

// camera
camera([0, 1, -3], [0, 0, 0], 45, [0, 1, 0]);

// lights
ambientlight(color [1,1,1] * 0.2);
pointlight([5, 5, -5], color [1,1,1] * 200);

// object
sphere(1.0) {
  translate [0, 0, 0];
  material {
    pigment solid(color [0.2, 0.6, 1.0]);
    finish(0.3, 0.3, 50.0, 0.1);
  };
}
```

Run:
- java -jar traci-0.0.1.jar -o renders/example.png -w 800 -h 600 example.traci

## Extending the system

Add a new built-in function:
- Implement it in `lang/interpreter/functions/BuiltinFunctions.java` (add to `ALL_BUILTIN_FUNCTIONS`)

Add a new object or keyword:
- Model the type under `model/` (e.g., new shape/material/texture)
- Map a language keyword to a factory in `lang/interpreter/node/ObjectNode` by extending `ObjectType`
- If entirely new syntax is needed, update grammars in `lang/grammar/*.g` and rebuild (`ant` regenerates parser sources)

Add a new statement or expression form:
- Update `TraciParser.g` (and possibly `TraciLexer.g`), regenerate sources, and add evaluation nodes under `lang/interpreter/node`

Modify rendering behavior:
- `render/Renderer.java` orchestrates threading; `render/Raytrace.java` contains ray-scene intersection and shading
- `model/*` controls materials, pigments, and light interaction

## Dependencies (vendored in lib/)

- ANTLR 3.5.2 (runtime and tool)
- Apache Commons CLI 1.3.1
- Anarres C Preprocessor 1.2.8
- JPLY 0.2.0 (PLY mesh reader)
- Apache Commons Lang 3.4
- JUnit 4.12 (tests only)

These are unpacked into the jar at build time for a self-contained executable.

## Repository tips for agents

- Code style: plain Java 7/8, no Lombok/streams; keep changes minimal and localized
- Parser generation: do not hand-edit generated `lang/parser/Traci*.java`; edit the grammars in `lang/grammar` and rebuild
- Determinism matters: RNGs are seeded for tile and builtins; preserve seeds unless you intend to change reproducibility
- Threading: rendering is embarrassingly parallel on work blocks; shared state is minimal (work queue, progress)
- Error messages: rely on `IncludeLocation` to preserve file/line/column across preprocessor includes

## Project metadata

- Version: 0.0.1 (see `build.xml`)
- License: see `LICENSE`
- Build tool: Apache Ant (`build.xml`)

## Known modernization opportunities (non-exhaustive)

- Replace Ant with Gradle/Maven while preserving ANTLR v3 generation
- Upgrade dependencies (ANTLR v4 would require grammar rework)
- Add headless CI build and test workflow
- Add sample scenes gallery and performance benchmarks
- Expand language docs and add a formal specification

---

This AGENTS.md was derived directly from the current codebase (October 2025). If you update core behavior (grammar, interpreter, renderer, CLI), please keep this document in sync.