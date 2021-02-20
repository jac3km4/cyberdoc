module Client (getDefinition, getIndex) where

import Data.Either (either)
import Effect.Aff (Aff, error, throwError)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Prelude (bind, pure, show, ($), (<<<), (<>))
import Simple.JSON as JSON
import Types (Definition, DefinitionIndex(..), Reference)

fetch :: M.Fetch
fetch = M.fetch windowFetch

getDefinition :: DefinitionIndex -> Aff Definition
getDefinition (DefinitionIndex index) = do
  res <- fetch url M.defaultFetchOptions
  body <- M.json res
  either (throwError <<< error <<< show) pure $ JSON.read body
  where
  url = M.URL $ "https://cors-proxy.jekky.workers.dev/?https://codeberg.org/adamsmasher/cyberpunk-api/raw/branch/master/" <> show index <> ".json"

getIndex :: Aff (Array Reference)
getIndex = do
  res <- fetch url M.defaultFetchOptions
  body <- M.json res
  either (throwError <<< error <<< show) pure $ JSON.read body
  where
  url = M.URL $ "https://cors-proxy.jekky.workers.dev/?https://codeberg.org/adamsmasher/cyberpunk-api/raw/branch/master/index.json"
