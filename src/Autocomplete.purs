module Autocomplete where

import Prelude
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Prismatic (Eff)
import Prismatic as P
import React (Children, ReactClass)
import React as React

type Props eff style r =
  ( items :: Array { name :: String | r }
  , onSelect :: eff Unit
  , showIcon :: Boolean
  , styling :: { | style }
  )

foreign import _component :: ∀ style r. ReactClass { children :: Children | Props (EffectFn1 { | r }) style r }

type Callback st act r a = { | r } -> Eff st act a

element :: ∀ st act style r. { | Props (Callback st act r) style r } -> P.Element st act
element { items, onSelect, styling, showIcon } interp _ = React.createElement _component { items, onSelect: onSelect', styling, showIcon } []
  where
  onSelect' = mkEffectFn1 (interp <<< onSelect)
