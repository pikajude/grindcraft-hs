{-# OPTIONS_GHC -ddump-splices #-}
{-# Language RecursiveDo #-}
{-# Language MultiWayIf #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language NamedFieldPuns #-}
{-# Language RecordWildCards #-}
{-# Language GADTs #-}
{-# Language QuasiQuotes #-}
{-# Language DataKinds #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Align
import Data.Bool
import Data.Either
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding ((<>))
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Time
import Debug.Trace
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers (keyDown, keyUp)
import GHCJS.DOM.KeyboardEvent
import Reflex.Dom
import Reflex.Dom.Contrib.CssClass

import Css

newtype Inventory = Inventory
    { unInventory :: Map Resource Int
    } deriving (Show)

newtype Ingredients = Ingredients
    { unIngredients :: Map Resource Int
    } deriving (Show, Monoid, Semigroup)

mapIngredients f (Ingredients i) = Ingredients (fmap f i)

ingredients = Just . Ingredients

allResourcesSet = S.fromDistinctAscList [minBound .. maxBound]

data Resource
    = Wood
    | WoodenAxe
    | Stick
    | Plank
    | Stone
    | StoneAxe
    | CraftingTable
    deriving (Show, Eq, Ord, Enum, Bounded)

data ProgressBar t = ProgressBar
    { pbActive :: Dynamic t Bool
    , pbComplete :: Event t ()
    }

data GameState t = GameState
    { inventoryDyn :: Dynamic t Inventory
    , hoveredItemIngredients :: Dynamic t Ingredients
    , ingredientsUsed :: Event t Ingredients
    , keysPressed :: Dynamic t (S.Set Text)
    , tick :: Event t Float
    }

data ResourceConfig t = ResourceConfig
    { resourceType :: Resource
    , resourceName :: Text
    , resourceNeeds :: Maybe Ingredients
    , resourceWants :: Maybe Ingredients
    , resourceDenomination :: Maybe Int
    , resourceImg :: Maybe Text
    , ingredientsControlVisibility :: Bool
    , craftTime :: Maybe (Dynamic t Float)
    , autoCraft :: Dynamic t Bool
    }

defaultResourceConfig :: ResourceConfig (SpiderTimeline Global)
defaultResourceConfig =
    ResourceConfig
        { resourceType = error "resourceType not set"
        , resourceName = error "resourceName not set"
        , resourceNeeds = Nothing
        , resourceWants = Nothing
        , resourceDenomination = Nothing
        , resourceImg = Nothing
        , ingredientsControlVisibility = False
        , craftTime = Nothing
        , autoCraft = constDyn False
        }

canMake ::
       (MonadReader (GameState t) m, Reflex t) => ResourceConfig t -> m (Dynamic t Bool)
canMake rsrc =
    case resourceReqs rsrc of
        Just (Ingredients n) -> do
            inven <- asks inventoryDyn
            return $
                ffor inven $ \(Inventory i) ->
                    all (>= 0) $ M.intersectionWith (\x y -> x - y) i n
        Nothing -> pure $ constDyn True

canMakeN rsrc =
    case resourceReqs rsrc of
        Nothing -> pure $ constDyn 1
        Just (Ingredients n) -> do
            inven <- asks inventoryDyn
            return $
                ffor inven $ \(Inventory i) ->
                    minimum $ M.intersectionWith (\x y -> x `div` y) i n

getPressedKeys target = do
    (e, io) <- newTriggerEvent
    _ <- f keyDown Right io
    _ <- f keyUp Left io
    foldDyn (either S.delete S.insert) (S.empty :: S.Set Text) e
  where
    f k r io = liftIO $ on target k $ liftIO . io . r =<< getKey =<< event

main :: IO ()
main =
    mainWidgetWithHead
        (do elAttr "style" (mconcat ["type" =: "text/css"]) $ text $ decodeUtf8 css
            elAttr
                "link"
                (mconcat
                     [ "rel" =: "stylesheet"
                     , "href" =: "https://meyerweb.com/eric/tools/css/reset/reset.css"
                     ]) $
                return ()
            elAttr
                "link"
                (mconcat
                     [ "rel" =: "stylesheet"
                     , "href" =: "https://fonts.googleapis.com/css?family=Lato:400,900"
                     ]) $
                return ()) $
    divClass "container" $ do
        ks <- getPressedKeys =<< askDocument
        t <- liftIO getCurrentTime
        ticks <- clockLossy (1 / 50) t
        rec let rsc = defaultResourceConfig
            craftables <-
                (`runReaderT` gameState) $
                divClass "inventory" $
                sequence
                    [ resource
                          rsc
                              { resourceName = "Wood"
                              , resourceType = Wood
                              , resourceImg =
                                    Just
                                        "https://d1u5p3l4wpay3k.cloudfront.net/minecraft_gamepedia/e/e4/Acacia_Log.png?version=bfc4eeaa677ea9a2e54c5c9e29ba206a"
                              , craftTime = Just $ constDyn 1
                              }
                    , resource
                          rsc
                              { resourceName = "Plank"
                              , resourceType = Plank
                              , resourceImg =
                                    Just
                                        "https://d1u5p3l4wpay3k.cloudfront.net/minecraft_gamepedia/b/bb/Acacia_Planks.png?version=af20f383251afeed48351e6e573473a2"
                              , resourceNeeds = ingredients (Wood =: 1)
                              , resourceDenomination = Just 4
                              }
                    , resource
                          rsc
                              { resourceName = "Crafting Table"
                              , resourceType = CraftingTable
                              , resourceNeeds = ingredients $ Plank =: 4
                              , resourceImg =
                                    Just
                                        "https://d1u5p3l4wpay3k.cloudfront.net/minecraft_gamepedia/d/d4/Crafting_Table.png?version=ef20591d7a264d60d6669f5f765e54e3"
                              }
                    , resource
                          rsc
                              { resourceName = "Stick"
                              , resourceType = Stick
                              , resourceNeeds = ingredients (Plank =: 2)
                              , resourceImg =
                                    Just
                                        "https://d1u5p3l4wpay3k.cloudfront.net/minecraft_gamepedia/a/aa/Stick.png?version=904de408b6779661deb6ea917324426e"
                              , resourceDenomination = Just 4
                              }
                    , resource
                          rsc
                              { resourceName = "Wooden Axe"
                              , resourceType = WoodenAxe
                              , resourceNeeds =
                                    ingredients $ mconcat [Plank =: 3, Stick =: 2]
                              , resourceWants = ingredients $ CraftingTable =: 1
                              , resourceImg =
                                    Just
                                        "https://d1u5p3l4wpay3k.cloudfront.net/minecraft_gamepedia/1/11/Wooden_Axe.png?version=81e7503c2a356cec0f9f738bbd0f92ad"
                              }
                    , resource
                          rsc
                              { resourceName = "Stone"
                              , resourceType = Stone
                              , resourceWants = ingredients $ WoodenAxe =: 1
                              , ingredientsControlVisibility = True
                              , resourceImg =
                                    Just
                                        "https://d1u5p3l4wpay3k.cloudfront.net/minecraft_gamepedia/6/67/Cobblestone.png?version=bd75abe38ce3d9309c351dbafc3881e2"
                              , craftTime = Just $ constDyn 1.5
                              }
                    , resource
                          rsc
                              { resourceName = "Stone Axe"
                              , resourceType = StoneAxe
                              , resourceNeeds =
                                    ingredients $ mconcat [Stone =: 3, Stick =: 2]
                              , resourceWants = ingredients $ CraftingTable =: 1
                              , resourceImg =
                                    Just
                                        "https://d1u5p3l4wpay3k.cloudfront.net/minecraft_gamepedia/2/2f/Stone_Axe.png?version=250cc757a31c16f4d9b72bde90c0fd69"
                              }
                    ]
            let (craftevs', craftovers', crafts) = unzip3 craftables
                craftevs = leftmost craftevs'
                craftovers = mconcat craftovers'
            inventoryI <-
                foldDyn
                    (flip $ foldr (uncurry $ M.insertWith (+)))
                    (M.fromSet (const 0) allResourcesSet) $
                mergeList crafts
            let gameState =
                    GameState
                        { inventoryDyn = Inventory <$> inventoryI
                        , ingredientsUsed = craftevs
                        , hoveredItemIngredients = craftovers
                        , tick = (1 / 50) <$ updated ticks
                        , keysPressed = ks
                        }
        el "div" $
            el "pre" $
            dynText $
            pack .
            M.showTreeWith (\k x -> show k ++ " := " ++ show x) False False .
            M.filter (> 0) . unInventory <$>
            inventoryDyn gameState

resourceReqs ResourceConfig {..} = salign resourceWants resourceNeeds

resourceClass r@ResourceConfig {..} cnt ctx = do
    m <- canMake r ctx
    Ingredients f <- hoveredItemIngredients ctx
    rc <- cnt
    return $
        mconcat
            [ singleClass "resource"
            , singleClass $ bool "nonbuyable" "buyable" m
            , cssClass $
              guard (not m && ingredientsControlVisibility) >> Just ("hidden" :: Text)
            , cssClass $
              ffor (M.lookup resourceType f) $ \n ->
                  if rc < n
                      then "needed"
                      else "has" :: Text
            ]

hoveredResourceRequirements r e =
    case resourceReqs r of
        Nothing -> return $ constDyn mempty
        Just n ->
            holdDyn mempty $
            leftmost [n <$ domEvent Mouseover e, mempty <$ domEvent Mouseout e]

progressBar ::
       ( DomBuilder t m
       , MonadHold t m
       , PostBuild t m
       , MonadSample t (Performable m)
       , TriggerEvent t m
       , PerformEvent t m
       , MonadFix m
       , Show a
       , RealFrac a
       , MonadIO (Performable m)
       )
    => Maybe (Dynamic t a)
    -> Dynamic t Bool
    -> Event t ()
    -> m (ProgressBar t)
progressBar Nothing _ craftClick = return $ ProgressBar (constDyn False) craftClick
progressBar (Just craftTime) autoCraft craftClick = do
    pb <- getPostBuild
    rec let craftStart = leftmost [craftClick, retrigger]
        craftEnd <-
            performEventAsync $
            craftStart $> \cb -> do
                cTime <- sample (current craftTime)
                void $
                    liftIO $
                    forkIO $ do
                        threadDelay $ round $ 1000000 * cTime
                        cb ()
        retrigger <-
            performEventAsync $
            leftmost [craftEnd, pb] $> \cb -> do
                crep <- sample (current autoCraft)
                liftIO $ when crep $ void $ forkIO $ threadDelay 50000 >> cb ()
    progressActive <- holdDyn False $ leftmost [True <$ craftStart, False <$ craftEnd]
    elDynAttr "div" (liftA2 mkClass craftTime progressActive) $ return ()
    return $ ProgressBar progressActive craftEnd
  where
    mkClass c p =
        mconcat
            [ "class" =:
              renderClass
                  (mconcat
                       [singleClass "progress-bar", singleClass $ bool "empty" "full" p])
            , "style" =: pack ("transition-duration: " ++ show c ++ "s")
            ]

resource ::
       (Reflex t, MonadWidget t m, MonadReader (GameState t) m)
    => ResourceConfig t
    -> m (Event t Ingredients, Dynamic t Ingredients, Event t (Resource, Int))
resource r@ResourceConfig {..}
    -- a pattern match matching on fields (like this one) forces the
    -- GameState to whnf by default. for early resources like wood and
    -- water, where we use the return value of this function to construct
    -- the initial GameState, we must not force it at all
 = do
    ~ctx@GameState {..} <- ask
    rec (e, pb) <-
            elDynKlass' "div" (resourceClass r rCount ctx) $ do
                forM_ resourceImg $ \r -> elAttr "img" ("src" =: r) $ return ()
                elClass "span" "badge upper" $ text resourceName
                elClass "span" "badge lower" $
                    display rCount
                progressBar craftTime autoCraft rClick
        let rClick = gate (not <$> current (pbActive pb)) $ domEvent Click e
            buyEvent =
                attachWithMaybe
                    (\(bc, kps) _ ->
                         if | bc < 1 -> Nothing
                            | "Shift" `S.member` kps -> Just bc
                            | otherwise -> Just 1)
                    (current $ zipDyn (canMakeN r ctx) keysPressed)
                    (pbComplete pb)
            rCountEv =
                leftmost
                    [ fmap (fromMaybe 1 resourceDenomination *) buyEvent
                    , negate <$>
                      fforMaybe ingredientsUsed (M.lookup resourceType . unIngredients)
                    ]
        rCount <- foldDyn (+) 0 rCountEv
    rhovered <- hoveredResourceRequirements r e
    return
        ( fmapMaybe id $
          ffor buyEvent $ \cnt -> fmap (mapIngredients (cnt *)) resourceNeeds
        , rhovered
        , (,) resourceType <$> rCountEv)
