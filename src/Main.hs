{-# OPTIONS_GHC -ddump-splices #-}
{-# Language RecursiveDo #-}
{-# Language MultiWayIf #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language NamedFieldPuns #-}
{-# Language RecordWildCards #-}
{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Align
import Data.Bool
import Data.Functor
import Data.List.NonEmpty
import Data.Map.Internal.Debug
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding ((<>))
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Time
import JSDOM.EventM (event, on)
import JSDOM.GlobalEventHandlers (keyDown, keyUp)
import JSDOM.KeyboardEvent
import JSDOM.Types (IsEventTarget, IsGlobalEventHandlers, MonadJSM, liftJSM)
import Language.Javascript.JSaddle.Warp
import Reflex.Dom hiding (mainWidgetWithHead, run)
import Reflex.Dom.Contrib.CssClass
import Reflex.Dom.Main

import Css
import Resource

type ResourceConfig' = ResourceConfig Resource

type Ingredients' = Ingredients Resource

newtype Inventory = Inventory
    { unInventory :: Map Resource Int
    } deriving (Show)

mapIngredients f (Ingredients i) = Ingredients (fmap f i)

ingredients = pure . Ingredients

allResourcesSet :: S.Set Resource
allResourcesSet = S.fromDistinctAscList [minBound .. maxBound]

data ProgressBar t = ProgressBar
    { pbActive :: Dynamic t Bool
    , pbComplete :: Event t ()
    }

data GameState t = GameState
    { inventoryDyn :: Dynamic t Inventory
    , hoveredItemIngredients :: Dynamic t (Ingredients')
    , ingredientsUsed :: Event t (Ingredients')
    , startingInventory :: Inventory
    , keysPressed :: Dynamic t (S.Set Text)
    , tick :: Event t Float
    }

canMake ::
       (MonadReader (GameState t) m, Reflex t) => ResourceConfig' t' -> m (Dynamic t Bool)
canMake = fmap (fmap (> 0)) . canMakeN

canMakeN ::
       (MonadReader (GameState t) m, Reflex t) => ResourceConfig' t' -> m (Dynamic t Int)
canMakeN rsrc =
    case resourceReqs rsrc of
        Nothing -> pure $ constDyn 1
        Just (Ingredients n) -> do
            inven <- asks inventoryDyn
            return $ ffor inven $ \(Inventory i) -> minimum $ M.intersectionWith div i n

getPressedKeys ::
       ( MonadFix m
       , MonadHold t m
       , TriggerEvent t m
       , Reflex t
       , IsGlobalEventHandlers e
       , IsEventTarget e
       , MonadJSM m
       )
    => e
    -> m (Dynamic t (S.Set Text))
getPressedKeys target = do
    (e, io) <- newTriggerEvent
    _ <- f keyDown Right io
    _ <- f keyUp Left io
    foldDyn (either S.delete S.insert) (S.empty :: S.Set Text) e
  where
    f k r io = liftJSM $ on target k $ liftIO . io . r =<< getKey =<< event

main :: IO ()
main =
    run 3579 $
    mainWidgetWithHead
        (do elAttr "style" (mconcat ["type" =: "text/css"]) $ text $ decodeUtf8 css
            el "title" $ text "Grindcraft"
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
        rec craftables <-
                (`runReaderT` gameState) $
                divClass "inventory" $ mapM resource $ allResources inventoryI
            let (craftevs', craftovers', crafts) = unzip3 craftables
                craftevs = leftmost craftevs'
                craftovers = mconcat craftovers'
            inventoryI <-
                foldDyn
                    (flip $ foldr (uncurry $ M.insertWith (+)))
                    (unInventory (startingInventory gameState) <>
                     M.fromSet (const 0) allResourcesSet) $
                mergeList crafts
            let gameState =
                    GameState
                        { inventoryDyn = Inventory <$> inventoryI
                        , ingredientsUsed = craftevs
                        , hoveredItemIngredients = craftovers
                        , startingInventory = Inventory $ Wood =: 15
                        , tick = (1 / 50) <$ updated ticks
                        , keysPressed = ks
                        }
        el "div" $
            el "pre" $
            dynText $
            pack .
            showTreeWith (\k x -> show k ++ " := " ++ show x) False False .
            M.filter (> 0) . unInventory <$>
            inventoryDyn gameState

resourceReqs :: ResourceConfig' t -> Maybe Ingredients'
resourceReqs ResourceConfig {..} = salign resourceWants resourceNeeds

resourceClass ::
       Reflex t
    => ResourceConfig' t'
    -> Dynamic t Int
    -> GameState t
    -> Dynamic t CssClass
resourceClass r@ResourceConfig {..} cnt ctx = do
    m <- canMake r ctx
    Ingredients f <- hoveredItemIngredients ctx
    rc <- cnt
    return $
        mconcat
            [ singleClass "resource"
            , singleClass $ bool "nonbuyable" "buyable" (m && craftable)
            , cssClass $ guard (not craftable) >> Just ("nocraft" :: Text)
            , cssClass $
              guard (not m && ingredientsControlVisibility) >> Just ("hidden" :: Text)
            , cssClass $
              ffor (M.lookup resourceType f) $ \n ->
                  if rc < n
                      then "needed"
                      else "has" :: Text
            ]

hoveredResourceRequirements ::
       ( MonadHold t m
       , HasDomEvent t target 'MouseoutTag
       , HasDomEvent t target 'MouseoverTag
       , Reflex t
       )
    => ResourceConfig' t'
    -> target
    -> m (Dynamic t Ingredients')
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
    => ResourceConfig' t
    -> m (Event t Ingredients', Dynamic t Ingredients', Event t (Resource, Int))
resource r@ResourceConfig {..}
    -- a pattern match matching on fields (like this one) forces the
    -- GameState to whnf by default. for early resources like wood and
    -- water, where we use the return value of this function to construct
    -- the initial GameState, we must not force it at all
 = do
    ~ctx@GameState {..} <- ask
    rec (e, pb) <-
            divClass "resource-wrapper" $
            elDynKlass' "div" (resourceClass r rCount ctx) $ do
                elAttr "img" ("src" =: resourceImg) $ return ()
                elClass "span" "badge upper" $ text resourceName
                elClass "span" "badge lower" $ display rCount
                progressBar craftTime autoCraft rClick
        let rClick =
                gate (not <$> current (pbActive pb)) $
                (if craftable
                     then domEvent Click e
                     else never)
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
        rCount <-
            foldDyn
                (+)
                (M.findWithDefault 0 resourceType $ unInventory startingInventory)
                rCountEv
    rhovered <- hoveredResourceRequirements r e
    return
        ( fmapMaybe id $
          ffor buyEvent $ \cnt -> fmap (mapIngredients (cnt *)) resourceNeeds
        , rhovered
        , (,) resourceType <$> rCountEv)
