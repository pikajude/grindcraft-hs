{-# Language GeneralizedNewtypeDeriving #-}

module Resource.Types where

import Data.List.NonEmpty
import qualified Data.Map as M
import Data.Map (Map)
import Data.Semigroup (Semigroup)
import Data.Text (Text)
import Reflex.Dom

newtype Ingredients r = Ingredients
    { unIngredients :: Map r Int
    } deriving (Show, Monoid, Semigroup)

data ResourceConfig r t = ResourceConfig
    { resourceType :: r
    , resourceName :: Text
    , resourceNeeds :: Maybe (Ingredients r)
    , resourceWants :: Maybe (Ingredients r)
    , resourceProduces :: Maybe (r, Maybe (Int, Int))
    , resourceDenomination :: Maybe Int
    , resourceImg :: Text
    , ingredientsControlVisibility :: Bool
    , craftable :: Bool
    , craftTime :: Maybe (Dynamic t Double)
    , autoCraft :: Dynamic t Bool
    }

defaultResourceConfig :: ResourceConfig a (SpiderTimeline Global)
defaultResourceConfig =
    ResourceConfig
        { resourceType = error "resourceType not set"
        , resourceName = error "resourceName not set"
        , resourceNeeds = Nothing
        , resourceWants = Nothing
        , resourceDenomination = Nothing
        , resourceImg = error "resourceImg not set"
        , ingredientsControlVisibility = False
        , craftTime = Nothing
        , craftable = True
        , autoCraft = constDyn False
        }

craftTimesOf ::
       (Eq a, Ord a, Monad m) => Double -> Map a Double -> m (Map a Int) -> m Double
craftTimesOf def times inven =
    ffor inven $ \i ->
        case M.filter (> 0) $ M.intersection times i of
            x
                | M.null x -> def
                | otherwise -> maximum $ M.elems x
