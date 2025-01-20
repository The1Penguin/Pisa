import Lean
import Lean.Compiler.IR
import Pisa.Json

namespace Pisa.Base

open Lean (Name ConstantInfo Environment ToJson toJson Expr)
open Lean.Elab.Command (CommandElabM)
open Lean.IR (findEnvDecl Decl FnBody IRType)

inductive Out
  | decT    : Expr → List Name → Decl → Out
  | dec     : Decl → Out
  | conInfo : ConstantInfo → Out

instance : ToJson Out where
  toJson
  | .decT typ l val => json% { "decT" : { "typ" : $typ, "level": $l, "def" : $val } }
  | .dec      val   => json% { "dec" : { "def" : $val } }
  | .conInfo  val   => json% { "conInfo" :  $val }

abbrev St := Std.HashMap Name Out
abbrev Search := ReaderT Environment <| StateT St <| Except String
def Search.runSt (env : Environment) (act : Search α) : Except String St :=
  Prod.snd <$> StateT.run (s := {}) (ReaderT.run (r := env) act)

partial def irTypeToName : IRType → Array Name
  | .struct (.some n) ts => ts.flatMap irTypeToName ++ #[n]
  | .union n ts => ts.flatMap irTypeToName ++ #[n]
  | _ => #[]

def exprToName : Lean.IR.Expr → Array Name
  | .ctor i _ => #[i.name]
  | .reuse _ i _ _ => #[i.name]
  | .fap c _ => #[c]
  | .pap c _ => #[c]
  | .box ty _ => irTypeToName ty
  | _ => #[]

partial def decRefs : FnBody → Array Name
  | .vdecl _ ty e b => irTypeToName ty ++ decRefs b ++ exprToName e
  | .jdecl _ xs v b => xs.flatMap (irTypeToName ·.ty) ++ decRefs v ++ decRefs b
  | .set _ _ _ b => decRefs b
  | .setTag _ _ b => decRefs b
  | .uset _ _ _ b => decRefs b
  | .sset _ _ _ _ ty b => irTypeToName ty ++ decRefs b
  | .inc _ _ _ _ b => decRefs b
  | .dec _ _ _ _ b => decRefs b
  | .del _ b => decRefs b
  | .mdata _ b => decRefs b
  | .case tid _ xType cs =>
    #[tid] ++
    cs.flatMap (λ | .ctor i b => #[i.name] ++ decRefs b
                  | .default b => decRefs b) ++
    irTypeToName xType
  | _ => #[]

def refs (es : Array Expr) : Array Name := es.flatMap (·.getUsedConstants)

open Out

partial def storeRefs (ns: Array Name) : Search Unit :=
  for n in ns do
    if (← get).contains n then continue

    let d := findEnvDecl (← read) n
    let m := (← read).find? n
    let c := match d, m with
        | .some c, .some n => some $ decT n.type n.levelParams c
        | .some c, _ => some $ dec c
        | .none, .some c => some $ conInfo c
        | _, _ => none
    let some c := c | .error s!"Unknown identifier {n}"

    modify (·.insert n c)
    storeRefs $ match c with
      | .decT t _ (.fdecl  _ _ _ body _) => decRefs body ++ refs #[t]
      | .decT t _ (.extern _ _ _ _     ) => refs #[t]
      | .dec (.fdecl  _ _ _ body _) => decRefs body
      | .dec (.extern _ _ _ _     ) => #[]
      | .conInfo (.axiomInfo  val)  => refs #[val.type]
      | .conInfo (.defnInfo   val)  => refs #[val.type, val.value]
      | .conInfo (.thmInfo    val)  => refs #[val.type, val.value]
      | .conInfo (.opaqueInfo val)  => refs #[val.type, val.value]
      | .conInfo (.quotInfo   val)  => refs #[val.type]
      | .conInfo (.ctorInfo   val)  => refs #[val.type]
      | .conInfo (.recInfo    val)  => refs <| #[val.type] ++ val.rules.map (·.rhs)
      | .conInfo (.inductInfo val)  => refs #[val.type] ++ val.ctors

def exportRoots
  (hdl : IO.FS.Stream) (env : Environment) (roots : Array (Name × String))
  : IO Unit := do
  hdl.putStrLn $ (toJson roots).compress
  let defs ← IO.ofExcept <| Search.runSt env <| storeRefs <| roots.map (·.fst)
  for ⟨_,e⟩ in defs do
    hdl.putStrLn $ (toJson e).compress

def findConjectures
  (env : Environment) (size : Option Nat) (roots : Array (Name × String))
  : IO (Array String) := do
  let (stdin, child) ← (·.takeStdin) =<< IO.Process.spawn {
    cmd := "pisa"
    args := (size.map (#["--size", toString ·])).getD #[]
    stdin := .piped
    stdout := .piped
    stderr := .piped
  }
  exportRoots (.ofHandle stdin) env roots

  let parsed := Lean.Json.parse (← child.stdout.readToEnd) >>= λo =>
    let data := o.getObjVal? "data"
    o.getObjVal? "success" >>= (·.getBool?) >>= λ
      | false => .error =<< data >>= (·.getStr?)
      | true => data >>= (·.getArr?) >>= (·.mapM (·.getStr?))

  match parsed with
    | .ok v =>
      -- let err ← child.stderr.readToEnd
      -- IO.println s!"stderr: {err}"
      return v
    | .error e =>
      let err ← child.stderr.readToEnd
      throw <| IO.userError <| s!"{e}\n\n{err}"

open Lean (logInfo Exception MonadError)

elab (name := «#pisa») "#pisa" n:num ? is:ident+ : command => do
  let resolve (id : Lean.Syntax) : CommandElabM Name := do
    -- If there are no matches the resolver will fail with context before head! does
    -- The first resolved const is the one used in an expr context, we want that
    let n := (← Lean.resolveGlobalConst id).head!
    Lean.Elab.addConstInfo id n none
    return n

  let ds ← match Search.runSt (← get).env <| storeRefs <| ← is.mapM resolve with
    | .error msg => throwError msg
    | .ok d => pure <| d.keys.foldl (· ++ "\n  " ++ toString ·) ""
  logInfo m!"Action for Pisa with size {(n.map (·.getNat)).getD 7} on{ds}"

open Lean.Lsp (CodeAction)
open Lean.Server (RequestM)
open Lean.CodeAction (CommandCodeAction)
open Lean.Elab (InfoTree)

partial def mkConjectures : Environment → Name → (Nat × List String) → String → (Nat × List String)
  | env, ns, (n, s), c =>
    let name := s!"conjecture{n}"
    match env.find? (.str ns name) with
    | .none => (n+1, s ++ [s!"theorem {name} {c} := sorry"])
    | _ => mkConjectures env ns (n+1, s) c

def mkFancyNames (env : Environment) (roots: Array Name)
                 (names: Lean.TSyntaxArray `ident) : Array (Name × String) :=
  let zipper := λr n => match env.find? r with
    | .some (.inductInfo b) =>
      let params := " α β γ δ ε ζ η θ ι κ".take (b.numParams * 2)
      let idxs := (List.replicate b.numIndices " _").foldl (· ++ ·) ""
      (r, s!"{n}{params}{idxs}")
    | .some _ => (r, n)
    | .none => unreachable!
  roots.zipWith zipper <| names.map (·.getId.toString)

@[command_code_action «#pisa»]
def insertConjectures : CommandCodeAction := λ _ snap _ node => do
  let .node info children := node | return #[]
  let some range := info.stx.getRange? | return #[]
  let doc ← RequestM.readDoc
  match info.stx with
    | `(#pisa $n ? $[$names:ident]* ) => do

      let getName (t : InfoTree) := match t with
        | .node (.ofTermInfo i) _ => match i.expr with
          | .const n _ => some n
          | _ => none
        | _ => none

      let some roots := children.toArray.mapM getName | return #[]

      let eager : CodeAction := {
        title := "Insert conjectures",
        kind? := "quickfix",
        isPreferred? := false
        edit? := none
      }
      return #[{
        eager,
        lazy? := some do
          let env := snap.cmdState.env
          let ns := snap.cmdState.scopes.head!.currNamespace

          -- TODO: Also send names info to preserve the names used in the command
          let fancyNames := mkFancyNames env roots names

          let conjs ← findConjectures env (n.map (·.getNat)) fancyNames
          let commands := List.foldl (·++·) "" <|
                          List.intersperse "\n" <|
                          Prod.snd <|
                          conjs.foldl (mkConjectures env ns) (0, [])

          pure { eager with
            edit? := some <| .ofTextDocumentEdit {
              textDocument := ⟨doc.meta.uri, none⟩
              edits := #[ {
                range := doc.meta.text.utf8RangeToLspRange range,
                newText := commands
              } ]
            }
          }
      } ]
    | _ => return #[]

elab "#pisas" n:num ? is:ident+ : command => do
  let resolve (id : Lean.Syntax) : CommandElabM Name := do
    -- If there are no matches the resolver will fail with context before head! does
    -- The first resolved const is the one used in an expr context, we want that
    let n := (← Lean.resolveGlobalConst id).head!
    Lean.Elab.addConstInfo id n none
    return n

  let roots := ← is.mapM resolve
  let env := (← get).env

  let fancyNames := mkFancyNames env roots is

  logInfo m!"For: {fancyNames}"
  let conjs ← findConjectures env (n.map (·.getNat)) fancyNames

  logInfo m!"Conjectures:{conjs.foldl (· ++ "\n" ++ ·) ""}"

def findConjectures2
  (env : Environment) (size : Option Nat) (roots : Array (Name × String))
  : IO (Array String) := do
  let (stdin, child) ← (·.takeStdin) =<< IO.Process.spawn {
    cmd := "pisa"
    args := (size.map (#["--size", toString ·])).getD #[]
    stdin := .piped
    stdout := .piped
    stderr := .piped
  }
  exportRoots (.ofHandle stdin) env roots

  let parsed := Lean.Json.parse (← child.stdout.readToEnd) >>= λo =>
    let data := o.getObjVal? "data"
    o.getObjVal? "success" >>= (·.getBool?) >>= λ
      | false => .error =<< data >>= (·.getStr?)
      | true => data >>= (·.getArr?) >>= (·.mapM (·.getStr?))

  match parsed with
    | .ok v =>
      let err ← child.stderr.readToEnd
      IO.println s!"stderr: {err}"
      return v
    | .error e =>
      let err ← child.stderr.readToEnd
      throw <| IO.userError <| s!"{e}\n\n{err}"

elab "#pisass" n:num ? is:ident+ : command => do
  let resolve (id : Lean.Syntax) : CommandElabM Name := do
    -- If there are no matches the resolver will fail with context before head! does
    -- The first resolved const is the one used in an expr context, we want that
    let n := (← Lean.resolveGlobalConst id).head!
    Lean.Elab.addConstInfo id n none
    return n

  let roots := ← is.mapM resolve
  let env := (← get).env

  let fancyNames := mkFancyNames env roots is

  logInfo m!"For: {fancyNames}"
  let conjs ← findConjectures2 env (n.map (·.getNat)) fancyNames

  logInfo m!"Conjectures:{conjs.foldl (· ++ "\n" ++ ·) ""}"
