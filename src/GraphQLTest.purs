module GraphQLTest (queryCollectibles) where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat (ResponseFormat(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Promise (fromAff, Promise)
import Data.Argonaut (class EncodeJson, encodeJson, jsonEmptyObject, (:=), (~>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Network.Ethereum.Types (Address)

-- -- import CSS as CSS
-- -- import CSS.Overflow as CSS
-- -- import Control.Coroutine as CR
-- -- import Control.Monad.List.Trans (foldl)
-- -- import Control.Promise (fromAff, Promise, toAff)
-- -- import Data.Array (replicate)
-- -- import Data.Either (Either(..), either)
-- -- import Data.Maybe (Maybe(..))
-- -- import Effect (Effect)
-- -- import Effect.Exception (throwException, error)
-- -- import Halogen (liftEffect)
-- -- import Halogen as H
-- -- import Halogen.Aff.Driver (HalogenIO)
-- -- import Halogen.Aff.Util (awaitLoad, selectElement, runHalogenAff)
-- -- import Halogen.HTML as HH
-- -- import Halogen.HTML.CSS as CSS
-- -- import Halogen.HTML.Events (onScroll)
-- -- import Halogen.HTML.Events as HE
-- -- import Halogen.Query.InputF (InputF(..))
-- -- import Halogen.VDom.Driver (runUI)
-- -- import Web.DOM.ParentNode (QuerySelector(..))

type Collectible = {
  tokenId :: Number,
  contractAddress :: Address,
  metadata :: Json
}

newtype GQLBody = GQLBody {
  query :: String
}
-- create a `EncodeJson` instance
instance encodeJsonGQLBody :: EncodeJson GQLBody where
  encodeJson (GQLBody x) = 
    "query" := x.query 
      ~> jsonEmptyObject


query :: GQLBody
query = GQLBody { query:"""
  query {
    allErc721Tokens(first:10) {
      nodes {
        tokenId
        contractAddress
        erc721MetadatumByTokenIdAndContractAddress {
          metadata
        }
      }
    }
  }
"""
}

queryCollectibles :: Effect (Promise Unit)
-- queryCollectibles :: Aff (Array String)
queryCollectibles = fromAff $ do
  res2 <- AX.post ResponseFormat.json "https://platform.pixura.io/api/graph" (RequestBody.json (encodeJson query))
  case res2.body of
    Left err -> log $ "POST /api response failed to decode: " <> AX.printResponseFormatError err
    Right json -> log $ "POST /api response: " <> J.stringify json