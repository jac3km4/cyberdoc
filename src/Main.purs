module Main where

import Autocomplete as Autocomplete
import Client as Client
import Data.Array (fold, intercalate)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Prelude (Unit, bind, const, discard, map, mempty, pure, show, ($), (<$>), (<<<), (<>), (=<<), (>))
import Prismatic as P
import Prismatic.HTML (mount)
import Prismatic.VDOM as H
import Prismatic.VDOM.Props as HP
import Types (Class, Definition(..), DefinitionIndex(..), Enum, Method, Reference, Type(..))
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget as ET
import Web.HTML as WE
import Web.HTML.Location as L
import Web.HTML.Window as W

data Action
  = LoadDefinition DefinitionIndex
  | LoadIndex

type AppState
  = { page :: PageState, index :: Array Reference }

data PageState
  = Loading
  | Loaded Definition
  | Index

main :: Effect Unit
main =
  runAff_ (log <<< show) do
    maybeId <- liftEffect getCurrentBrowserRoute
    index <- Client.getIndex
    state <- case maybeId of
      Just id -> do
        definition <- Client.getDefinition id
        pure { page: Loaded definition, index }
      Nothing -> pure { page: Index, index }
    liftEffect $ mount "main" (P.element app) state

app :: P.Component AppState Action
app = P.wired $ (P.defaultSpec render performAction) { componentDidMount = componentDidMount }
  where
  render state =
    H.div'
      [ renderNav state.index
      , H.div [ HP.className "container page" ]
          [ renderPage state ]
      ]

  performAction _ (LoadDefinition index) = do
    P.modify (_ { page = Loading })
    def <- liftAff $ Client.getDefinition index
    P.modify (_ { page = Loaded def })
  performAction _ LoadIndex = P.modify (_ { page = Index })

  componentDidMount interp = liftEffect do
    listener <- ET.eventListener (const $ interp onLocationChange)
    target <- W.toEventTarget <$> WE.window
    ET.addEventListener (EventType "hashchange") listener false target

  onLocationChange = do
    id <- liftEffect getCurrentBrowserRoute
    traverse_ (P.dispatch <<< LoadDefinition) id

renderPage :: ∀ s. AppState -> P.Element s Action
renderPage { page, index } = case page of
  Loading ->
    H.div [ HP.className "row" ]
      [ H.div [ HP.className "column column-50 column-offset-50" ] [ H.h3' [ H.text "Loading" ] ] ]
  Loaded (ClassDefinition class') -> renderClass class'
  Loaded (FunctionDefinition fun) -> renderMethod fun
  Loaded (EnumDefinition enum) -> renderEnum enum
  Index -> renderIndex index

renderIndex :: ∀ s. Array Reference -> P.Element s Action
renderIndex refs =
  H.section [ HP.className "section" ]
    [ H.h2 [ HP.className "title" ] [ H.text "Index" ]
    , H.ul [] $ map renderReference $ Array.take 50 refs
    ]
  where
  renderReference ref = H.li' [ renderLink ref.name ref.index ]

renderClass :: ∀ s. Class -> P.Element s Action
renderClass class' =
  H.div'
    [ H.h3'
      [ H.text $ class'.visibility
      , H.text " "
      , if class'.isNative then H.text "native " else mempty
      , H.text $ if class'.isStruct then "struct " else "class "
      , H.text class'.name
      ]
    , renderBases class'.bases
    , if Array.length class'.fields > 0 then H.h4' [ H.text "fields" ] else mempty
    , H.ul [] $ renderField <$> class'.fields
    , if Array.length class'.methods > 0 then H.h4' [ H.text "methods" ] else mempty
    , H.ul [] $ renderMethod <$> class'.methods
    ]
  where
  renderBases bases = case Array.uncons bases of
    Just { head, tail } ->
      H.dl'
        [ H.dd'
            [ H.text "extends "
            , renderLink head.name head.index
            , renderBases tail
            ]
        ]
    Nothing -> mempty

  renderField field =
    H.li'
      [ H.code'
          [ H.text field.name
          , H.text ": "
          , renderType field.type
          ]
      ]

renderMethod :: ∀ s. Method -> P.Element s Action
renderMethod method =
  let prettyName = Array.head (String.split (Pattern ";") method.name)
  in
    H.li [ HP.title method.name ]
      [ H.code'
          [ H.text method.visibility
          , H.text " "
          , if method.isStatic then H.text "static " else mempty
          , if method.isNative then H.text "native " else mempty
          , H.text $ fold prettyName
          , H.text "("
          , intercalate (H.text ", ") $ renderParameter <$> method.parameters
          , H.text ")"
          , H.text ": "
          , maybe (H.text "Void") renderType method.returnType
          ]
      ]
  where
  renderParameter param =
    H.span'
      [ if param.isOptional then H.text "opt " else mempty
      , if param.isOut then H.text "out " else mempty
      , H.text param.name
      , H.text ": "
      , renderType param.type
      ]

renderType :: ∀ s. Type -> P.Element s Action
renderType type' = printType type'
  where
  printType (Basic { name }) = H.text name
  printType (Class { name, index }) = renderLink name index
  printType (Ref { inner }) = printType inner
  printType (WeakRef { inner }) = printType inner
  printType (ScriptRef { inner }) = printType inner
  printType (Array { inner }) = H.text "array<" <> printType inner <> H.text ">"
  printType (StaticArray { inner, size }) = printType inner <> H.text ("[" <> show size <> "]")

renderEnum :: ∀ s. Enum -> P.Element s Action
renderEnum enum =
  H.div'
    [ H.h3' [ H.text $ "enum " <> enum.name ]
    , H.ul [] $ renderEnumValue <$> enum.members
    ]
  where
  renderEnumValue member =
    H.li []
      [ H.text member.name
      , H.text " = "
      , H.text $ show member.value
      ]

renderNav :: ∀ s. Array Reference -> P.Element s Action
renderNav index =
  H.nav [ HP.className "navigation" ]
    [ H.section [ HP.className "container" ]
        [ H.a [ HP.className "navigation-title", HP.onClick $ const $ P.dispatch LoadIndex ]
            [ H.text "Cyberdoc" ]
        , H.ul [ HP.className "navigation-list float-right" ]
            [ H.li [ HP.className "navigation-item" ]
                [ H.section [ HP.className "column" ]
                    [ autocomplete ]
                ]
            ]
        ]
    ]
  where
  autocomplete = Autocomplete.element { items: index, onSelect: onOptionSelect, styling: {}, showIcon: false }

  onOptionSelect option = do
    let DefinitionIndex id = option.index
    liftEffect $ L.setHash (show id) =<< W.location =<< WE.window

renderLink :: ∀ st. String -> DefinitionIndex -> P.Element st Action
renderLink name index =
  let DefinitionIndex id = index
  in H.a [ HP.href $ "#" <> show id ] [ H.text name ]

getCurrentBrowserRoute :: Effect (Maybe DefinitionIndex)
getCurrentBrowserRoute = do
  hash <- L.hash =<< W.location =<< WE.window
  pure $ map DefinitionIndex $ Int.fromString $ String.drop 1 hash
