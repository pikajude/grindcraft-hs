{-# Language CPP #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Resource.Types
    ( Ingredients(..)
    , ResourceConfig(..)
    , defaultResourceConfig
    , craftTimesOf
    ) where

import Data.List.NonEmpty
import qualified Data.Map as M
import Data.Map (Map)
import Data.Semigroup (Semigroup)
import Data.Set (Set)
import Data.Text (Text)
import Natural
import Reflex.Dom

newtype Ingredients r = Ingredients
    { unIngredients :: Map r Natural
    } deriving (Show, Monoid, Semigroup)

data ResourceConfig r f e t = ResourceConfig
    { resourceType :: r
    , resourceName :: Text
    , resourceNeeds :: Map r Natural
    , resourceWants :: [Set r]
    , resourceProduces :: Maybe (r, Maybe (Int, Int))
    , resourceDenomination :: Maybe Natural
    , resourceImg :: Text
    , resourcePrim :: f (e, Integer)
    , ingredientsControlVisibility :: Bool
    , craftable :: Bool
    , craftTime :: Maybe (Dynamic t Double)
    , autoCraft :: Dynamic t Bool
    }

defaultResourceConfig :: (Ord a, Reflex t) => ResourceConfig a Maybe e t
defaultResourceConfig =
    ResourceConfig
        { resourceType = error "resourceType not set"
        , resourceName = error "resourceName not set"
        , resourceNeeds = mempty
        , resourceWants = []
        , resourceProduces = Nothing
        , resourceDenomination = Nothing
        , resourceImg = error "resourceImg not set"
        , ingredientsControlVisibility = False
        , craftTime = Nothing
        , craftable = True
        , autoCraft = constDyn False
        }

craftTimesOf ::
       (Eq a, Ord a, Monad m) => Double -> Map a Double -> m (Map a Natural) -> m Double
craftTimesOf def times inven =
    ffor inven $ \i ->
        case M.intersection times $ M.filter (> 0) i of
            x
                | M.null x -> def
                | otherwise -> minimum $ M.elems x
