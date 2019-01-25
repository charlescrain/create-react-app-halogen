module Test (test, halogenApp, Query) where

import Prelude

import Control.Promise (fromAff, Promise, toAff)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff, throwError)
import Effect.Exception (throwException, error)
import Halogen as H
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Util (awaitLoad, selectElement, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.InputF (InputF(..))
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

data Query a = ToggleState a

type State = { on :: Boolean }

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { on: false }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Hello world!" ]
      , HH.p_
          [ HH.text "Why not toggle this button:" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text
              if not state.on
              then "Don't push me"
              else "I said don't push me!"
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ToggleState next -> do
      _ <- H.modify (\state -> { on: not state.on })
      pure next

test :: String -> String
test st = st <> " hi!"

type JSHandler = { clickButton :: Effect (Promise Unit) }

halogenApp :: Effect (Promise JSHandler)
halogenApp = fromAff $ do
  awaitLoad
  mhApp <- selectElement (QuerySelector "#halogenApp")
  case mhApp of
    Nothing -> throwError $ error "error"
    Just hApp -> do
      io <- runUI component unit hApp
      pure { clickButton: fromAff $ (io.query $ H.action $ ToggleState) }
