import Lean
import Lean.Compiler.IR
import Lean.Compiler.LCNF
import Lean.Data.Json.FromToJson

open Lean
namespace Pisa.Json

instance : ToJson Literal where
  toJson
  | .natVal val => .num val
  | .strVal val => .str val

#eval do
  let _ ← Elab.Deriving.FromToJson.mkToJsonInstanceHandler #[
    ``String.Pos,
    ``Substring,
    ``SourceInfo,
    ``Syntax.Preresolved,
    ``Syntax
  ]

instance : ToJson DataValue where
  toJson
  | .ofString v => json% { "string" : $v }
  | .ofBool   v => json% { "bool" : $v }
  | .ofName   v => json% { "name" : $v }
  | .ofNat    v => json% { "nat" : $v }
  | .ofInt    v => json% { "int" : $v }
  | .ofSyntax v => json% { "syntax" : $v }

instance : ToJson UInt32 := ⟨(·.toNat)⟩
instance : ToJson LMVarId := ⟨(toJson ·.name)⟩
instance : ToJson MData := ⟨(toJson ·.entries)⟩

-- XXX: Should be a better way to do this
--      We're basically invoking `deriving ToJson` after the fact
#eval do
  let _ ← Elab.Deriving.FromToJson.mkToJsonInstanceHandler #[
    ``BinderInfo,
    ``QuotKind,
    ``Level,
    ``Expr,
    ``RecursorRule,
    ``ReducibilityHints,
    ``DefinitionSafety,
    ``AxiomVal,
    ``DefinitionVal,
    ``TheoremVal,
    ``OpaqueVal,
    ``QuotVal,
    ``InductiveVal,
    ``ConstructorVal,
    ``RecursorVal,
    ``ConstantInfo
  ]

instance : ToJson (Name × ConstantInfo) where
  toJson
    | (n, ci) => if n != ci.name
      then panic! s!"Name incongrunece in env: ${n} != ${ci.name}"
      else match ci with
        | .axiomInfo  v => json% { "axiomInfo"  : $v }
        | .defnInfo   v => json% { "defnInfo"   : $v }
        | .thmInfo    v => json% { "thmInfo"    : $v }
        | .opaqueInfo v => json% { "opaqueInfo" : $v }
        | .quotInfo   v => json% { "quotInfo"   : $v }
        | .inductInfo v => json% { "inductInfo" : $v }
        | .ctorInfo   v => json% { "ctorInfo"   : $v }
        | .recInfo    v => json% { "recInfo"    : $v }

open Compiler

deriving instance ToJson for IR.VarId
deriving instance ToJson for IR.IRType
deriving instance ToJson for IR.CtorInfo
deriving instance ToJson for IR.Arg
deriving instance ToJson for IR.LitVal
deriving instance ToJson for IR.Expr
deriving instance ToJson for IR.JoinPointId
deriving instance ToJson for IR.Param
deriving instance ToJson for String.Pos
deriving instance ToJson for Substring
deriving instance ToJson for SourceInfo
deriving instance ToJson for Syntax.Preresolved
deriving instance ToJson for Syntax
deriving instance ToJson for DataValue
deriving instance ToJson for KVMap
deriving instance ToJson for IR.AltCore
deriving instance ToJson for IR.FnBody
deriving instance ToJson for IR.DeclInfo
deriving instance ToJson for ExternEntry
deriving instance ToJson for ExternAttrData
deriving instance ToJson for IR.Decl
