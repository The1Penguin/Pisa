import Pisa.Base
import Pisa.Json

namespace Pisa.Exe

def help (name : String) : String :=
s!"Export Lean AST as JSON of select constructs

This is the abstract syntax of the elaborated lambda calculus,
not the concrete syntax we typically interract with with.

Usage:
  {name} ⟨module⟩+ : ⟨id(=alias)⟩+

Arguments:
  <module>     A fully qualified module path
  <id(=alias)> A fully qualified ideitifier available within these modules
               Optionally with an alias to be used in the generated conjectures

Example:
  Pisa.Base Pisa.Exe : Search Pisa.Base.St main
"
namespace Args

open Lean Elab

def mkEnv (src : String) : IO Environment := do
  let inputCtx := Parser.mkInputContext src "<anonymous>"
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← Elab.processHeader header {} messages inputCtx
  let s ← IO.processCommands inputCtx parserState (Command.mkState env messages)
  return s.commandState.env

/-- Syntax to help parse identifiers in cli args -/
declare_syntax_cat renamed
syntax (name := stxRename) ident "=" ident : renamed
syntax (name := stxNoRename) ident : renamed

declare_syntax_cat args
syntax (name := stxArgs) ident+ ":" renamed+ : args

def argsSyntax :=
s!"namespace Pisa.Exe.Args
declare_syntax_cat renamed
syntax (name := stxRename) ident \"=\" ident : renamed
syntax (name := stxNoRename) ident : renamed

declare_syntax_cat args
syntax (name := stxArgs) ident+ \":\" renamed+ : args
"
def parse (args : List String) : IO (Array Name × Array (Name × String)) := do
  let env ← mkEnv argsSyntax
  let stx := Parser.runParserCategory env `args (" ".intercalate args)
  IO.ofExcept <| do match ← stx with
    | `(args|$[$ms:ident]* : $[$is:renamed]*) =>
      (ms.map Syntax.getId, ·) <$> is.mapM λ
        | `(renamed|$i:ident) => .ok (i.getId, i.getId.toString)
        | `(renamed|$i:ident = $n:ident) => .ok (i.getId, n.getId.toString)
        | _ => .error "Unexpected rename rule"
    | s => .error s!"Unexpected args rule {s}"

end Args

open Pisa.Base (exportRoots)
open Lean (
  Import
  Name
  findSysroot
  importModules
  initSearchPath
  searchPathRef
  toJson
)

def main (args : List String) : IO Unit := do
  match args with
    | [] | ["--help"] =>
      IO.print $ help $ (← IO.appPath).fileName.getD "exe"
    | _ =>
      match (← IO.getEnv "LEAN_SRC_PATH") with
        | none => -- TODO: imitate Lake.CLI.env more robustly
          -- searchPathRef.modify ("./." :: ·)
          let child ← IO.Process.spawn {
            cmd := "lake"
            args := #[ "env", (← IO.appPath).toString ] ++ args.toArray
          }
          let c ← child.wait
          if c != 0 then .error s!"lake env failed with code {c}"

        | _ =>
          initSearchPath (← findSysroot)
          let (modules, renames) ← Args.parse args

          -- TODO: Seems there are no great ways to load source files
          --       Since the cli is not the primary way to interface anyways
          --       this may be good enough
          let child ← IO.Process.spawn {
            cmd := "lake"
            args := #[ "build" ] ++ modules.map (·.toString)
            stdout := .piped,
          }
          (← IO.getStderr).putStr (← child.stdout.readToEnd)
          let c ← child.wait
          if c != 0 then .error s!"lake build failed with code {c}"

          let env ← importModules (modules.map Coe.coe) {}
          exportRoots (← IO.getStdout) env renames

end Pisa.Exe

abbrev main := Pisa.Exe.main
