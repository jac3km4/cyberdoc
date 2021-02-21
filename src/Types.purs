module Types where

import Control.Monad.Error.Class (throwError)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe)
import Foreign (F, ForeignError(..))
import Prelude ((<$>), bind, ($))
import Simple.JSON (class ReadForeign, readImpl)

data Definition
  = ClassDefinition Class
  | FunctionDefinition Method
  | EnumDefinition Enum

instance readDefinition :: ReadForeign Definition where
  readImpl f = do
    obj <- readTag
    case obj.tag of
      "Class" -> ClassDefinition <$> readImpl f
      "Function" -> FunctionDefinition <$> readImpl f
      "Enum" -> EnumDefinition <$> readImpl f
      _ -> throwError $ NEL.singleton $ ForeignError "Invalid definiton"
    where
    readTag :: F { tag :: String }
    readTag = readImpl f

type Reference
  = { name :: String
    , index :: DefinitionIndex
    }

newtype DefinitionIndex
  = DefinitionIndex Int

instance readDefinitionIndex :: ReadForeign DefinitionIndex where
  readImpl f = DefinitionIndex <$> readImpl f

data Type
  = Basic { name :: String }
  | Class { name :: String, index :: DefinitionIndex }
  | Ref { inner :: Type }
  | WeakRef { inner :: Type }
  | ScriptRef { inner :: Type }
  | Array { inner :: Type }
  | StaticArray { size :: Int, inner :: Type }

instance readType :: ReadForeign Type where
  readImpl f = do
    obj <- readKind
    case obj.kind of
      "Prim" -> Basic <$> readImpl f
      "Class" -> Class <$> readImpl f
      "Ref" -> Ref <$> readImpl f
      "WeakRef" -> WeakRef <$> readImpl f
      "ScriptRef" -> ScriptRef <$> readImpl f
      "Array" -> Array <$> readImpl f
      "StaticArray" -> StaticArray <$> readImpl f
      _ -> throwError $ NEL.singleton $ ForeignError "Invalid definiton"
    where
    readKind :: F { kind :: String }
    readKind = readImpl f

type Class
  = { name :: String
    , visibility :: String
    , bases :: Array Reference
    , fields :: Array Field
    , methods :: Array Method
    , isAbstract :: Boolean
    , isFinal :: Boolean
    , isNative :: Boolean
    , isStruct :: Boolean
    }

type Field
  = { name :: String
    , type :: Type
    , isNative :: Boolean
    , isEdit :: Boolean
    , isInline :: Boolean
    , isConst :: Boolean
    , isRep :: Boolean
    , isPersistent :: Boolean
    }

type Method
  = { name :: String
    , parameters :: Array Parameter
    , returnType :: Maybe Type
    , visibility :: String
    , isStatic :: Boolean
    , isFinal :: Boolean
    , isExec :: Boolean
    , isCallback :: Boolean
    , isNative :: Boolean
    , source :: Maybe String
    }

type Parameter
  = { name :: String
    , type :: Type
    , isOut :: Boolean
    , isOptional :: Boolean
    }

type Enum
  = { name :: String
    , members :: Array EnumMember
    }

type EnumMember
  = { name :: String, value :: Int }
