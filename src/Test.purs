module Test (test, halogenApp, Query) where

import Prelude

import CSS as CSS
import CSS.Overflow as CSS
import Control.Coroutine as CR
import Control.Monad.List.Trans (foldl)
import Control.Promise (fromAff, Promise, toAff)
import Data.Array (replicate)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff, throwError)
import Effect.Exception (throwException, error)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Util (awaitLoad, selectElement, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events (onScroll)
import Halogen.HTML.Events as HE
import Halogen.Query.InputF (InputF(..))
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import GraphQLTest (queryCollectibles)

-- Types the govern events into and within the Component
-- Input 
data Query a = ToggleState a
            --  | NewRoute String a

-- Output
data Message = Toggled Boolean

type State = { on :: Boolean }


emptyBlock :: _
emptyBlock =  HH.div [CSS.style $ do  
                        CSS.backgroundColor CSS.red 
                        CSS.width (CSS.px 100.0 )
                        CSS.height (CSS.px 100.0)
                        CSS.position CSS.relative
                        CSS.display CSS.inlineBlock] [] 

component :: forall m. H.Component HH.HTML Query Unit Message m
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
      , HH.div [ HE.onScroll (HE.input_ ToggleState)
               , CSS.style $ do 
                    CSS.height (CSS.px 100.0)
                    CSS.width (CSS.px 100.0)
                    CSS.overflow CSS.scroll
               ] 
               (replicate 10 emptyBlock)
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    ToggleState next -> do
      _ <- H.modify (\state -> { on: not state.on })
      state <- H.get
      H.raise $ Toggled state.on
      pure next

test :: String -> String
test st = st <> " hi!"

type JSHandler = { clickButton :: Effect (Promise Unit)
                 , clickEvent :: Effect Unit -> Effect (Promise Unit) 
                 }

halogenApp :: Effect (Promise JSHandler)
halogenApp = fromAff $ do
  awaitLoad
  mhApp <- selectElement (QuerySelector "#halogenApp")
  case mhApp of
    Nothing -> throwError $ error "error"
    Just hApp -> do
      io <- runUI component unit hApp
      pure { clickButton: fromAff $ (io.query $ H.action $ ToggleState) 
           , clickEvent: \f -> fromAff $ io.subscribe $ CR.consumer (\_ -> do 
                                                                      liftEffect f
                                                                      pure Nothing
                                                                    )
           }

