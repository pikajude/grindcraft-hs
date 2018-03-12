{-# OPTIONS_GHC -ddump-splices #-}
{-# Language CPP #-}
{-# Language ScopedTypeVariables #-}
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
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import Data.Map.Strict (Map, fromList, showTreeWith)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Time
import Debug.Trace
import Language.Javascript.JSaddle.Warp
import Reflex.Dom hiding (mainWidgetWithHead, run)
import Reflex.Dom.Contrib.CssClass
import Reflex.Dom.Main
import System.Random

import Css
import Resource
import Natural

#ifdef ghcjs_HOST_OS
import GHCJS.DOM.EventM (event, on)
import GHCJS.DOM.GlobalEventHandlers (keyDown, keyUp)
import GHCJS.DOM.KeyboardEvent
import GHCJS.DOM.Types (IsEventTarget, IsGlobalEventHandlers, MonadJSM, liftJSM)
#else
import JSDOM.EventM (event, on)
import JSDOM.GlobalEventHandlers (keyDown, keyUp)
import JSDOM.KeyboardEvent
import JSDOM.Types (IsEventTarget, IsGlobalEventHandlers, MonadJSM, liftJSM)
#endif

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = ffor

lookupSet k ((p, v):xs)
    | k `elem` p = Just v
    | otherwise = lookupSet k xs
lookupSet _ [] = Nothing

data ResourceEffects t = ResourceEffects
    { resourcesConsumed :: Event t (Map Resource Natural)
    , ingredientsOfHoveredThing :: Dynamic t [(NonEmpty Resource, Natural)]
    , resourceTotal :: Production t
    }

data Production t
    = ProducesSelf (Event t (Resource, Int))
    | ProducesOther (Event t (Resource, Int))

unProd (ProducesSelf e) = e
unProd (ProducesOther e) = e

type ResourceConfig' = ResourceConfig Resource

type Ingredients' = Ingredients Resource

newtype Inventory = Inventory
    { unInventory :: Map Resource Natural
    } deriving (Show)

mapIngredients f (Ingredients i) = Ingredients (fmap f i)

allResourcesSet :: S.Set Resource
allResourcesSet = S.fromDistinctAscList [minBound .. maxBound]

data ProgressBar t = ProgressBar
    { pbActive :: Dynamic t Bool
    , pbComplete :: Event t ()
    }

data GameState t = GameState
    { inventoryDyn :: Dynamic t Inventory
    , hoveredItemIngredients :: Dynamic t [(NonEmpty Resource, Natural)]
    , ingredientsUsed :: Event t (Map Resource Natural)
    , startingInventory :: Inventory
    , keysPressed :: Dynamic t (S.Set Text)
    , tick :: Event t Float
    }

insertInts :: NonEmpty (Resource, Int) -> Map Resource Natural -> Map Resource Natural
insertInts =
    flip $
    foldr
        (\(r, n) ->
             M.insertWith
                 (if n < 0
                      then subtract
                      else (+))
                 r
                 (fromIntegral $ abs n))

canMake ::
       (MonadReader (GameState t) m, Reflex t) => ResourceConfig' t' -> m (Dynamic t Bool)
canMake = fmap (fmap (> 0)) . canMakeN

canMakeN ::
       (MonadReader (GameState t) m, Reflex t)
    => ResourceConfig' t'
    -> m (Dynamic t Natural)
canMakeN rsrc = do
    let (s, m) = resourceReqs rsrc
    inven <- asks inventoryDyn
    return $
        inven <&> \(Inventory i) ->
            if all (hasOneOf i) s
                then if M.null m
                         then 1
                         else minimum $ M.intersectionWith div i m
                else 0
  where
    hasOneOf inv rset = any (\r -> M.findWithDefault 0 r inv > 0) rset

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
        (do elAttr "style" ("type" =: "text/css") $ text $ decodeUtf8 css
            el "title" $ text "Grindcraft"
            elAttr
                "link"
                (fromList
                     [ ("rel", "stylesheet")
                     , ("href", "https://meyerweb.com/eric/tools/css/reset/reset.css")
                     ]) $
                return ()
            elAttr
                "link"
                (fromList
                     [ ("rel", "stylesheet")
                     , ("href", "https://fonts.googleapis.com/css?family=Lato:400,900")
                     ]) $
                return ()) $
    divClass "container" $ do
        ks <- getPressedKeys =<< askDocument
        t <- liftIO getCurrentTime
        ticks <- clockLossy (1 / 50) t
        rec primMined <- runReaderT (primResource $ primResources inventoryI) gameState
            craftables <-
                (`runReaderT` gameState) $
                divClass "inventory" $ mapM resource $ allResources inventoryI
            let (craftevs', craftovers', crafts') =
                    ( map resourcesConsumed craftables
                    , map ingredientsOfHoveredThing craftables
                    , map (unProd . resourceTotal) craftables)
                craftevs = leftmost craftevs'
                craftovers = mconcat craftovers'
                crafts = ffor primMined (\p -> (p, 1)) : crafts'
            inventoryI <-
                foldDyn
                    insertInts
                    (unInventory (startingInventory gameState) <>
                     M.fromSet (const 0) allResourcesSet) $
                mergeList crafts
            let gameState =
                    GameState
                        { inventoryDyn = Inventory <$> inventoryI
                        , ingredientsUsed = craftevs
                        , hoveredItemIngredients = craftovers
                        , startingInventory = Inventory mempty
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

resourceReqs :: ResourceConfig' t -> ([S.Set Resource], Map Resource Natural)
resourceReqs ResourceConfig {..} = (resourceWants, resourceNeeds)

resourceClass ::
       Reflex t
    => ResourceConfig' t'
    -> Dynamic t Natural
    -> GameState t
    -> Dynamic t CssClass
resourceClass r@ResourceConfig {..} cnt ctx = do
    m <- canMake r ctx
    f <- hoveredItemIngredients ctx
    rc <- cnt
    return $
        mconcat
            [ singleClass "resource"
            , singleClass $ bool "nonbuyable" "buyable" (m && craftable)
            , cssClass $ guard (not craftable) >> Just ("nocraft" :: Text)
            , cssClass $
              guard (not m && ingredientsControlVisibility) >> Just ("hidden" :: Text)
            , cssClass $
              lookupSet resourceType f <&> \n ->
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
    -> m (Dynamic t [(NonEmpty Resource, Natural)])
hoveredResourceRequirements r e =
    holdDyn mempty $
    leftmost [(rlist ++ mp') <$ domEvent Mouseover e, mempty <$ domEvent Mouseout e]
  where
    (sets, mp) = resourceReqs r
    rlist = map (\s -> (N.fromList (S.toList s), 1)) sets
    mp' = map (\(k, v) -> (k :| [], v)) $ M.toList mp

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
        fromList
            [ ( "class"
              , renderClass
                    (mconcat
                         [singleClass "progress-bar", singleClass $ bool "empty" "full" p]))
            , ("style", pack ("transition-duration: " ++ show c ++ "s"))
            ]

primResource ::
       (MonadReader (GameState t) m, MonadWidget t m)
    => [ResourceConfig' t]
    -> m (Event t Resource)
primResource rs = do
    ctx <- ask
    x <- workflowView $ go ctx rs (head rs)
    switchHoldPromptly never x
  where
    findCraftable ctx rs = do
        ms <-
            sequence $
            ffor rs $ \r ->
                sample . current $
                ffor (canMake r ctx) $ \c ->
                    if c
                        then Just r
                        else Nothing
        let choices = catMaybes ms
        idx <- liftIO $ randomRIO (1, length choices)
        return $ choices !! (idx - 1)
    go ctx rs r@ResourceConfig {..} =
        Workflow $ do
            rec (e, pb) <-
                    divClass "resource-wrapper" $
                    elDynKlass' "div" (resourceClass r rCount ctx) $ do
                        elAttr "img" ("src" =: resourceImg) $ return ()
                        elClass "span" "badge upper" $ text resourceName
                        elClass "span" "badge lower" $ display rCount
                        progressBar craftTime autoCraft rClick
                let canStartCrafting
                        | craftable =
                            zipDynWith (\a m -> not a && m) (pbActive pb) (canMake r ctx)
                        | otherwise = constDyn False
                    rClick = gate (current canStartCrafting) (domEvent Click e)
                    buyEvent = 1 <$ pbComplete pb
                    rCount = constDyn 0
                produceNext <- performEvent $ buyEvent $> findCraftable ctx rs
            return (resourceType <$ buyEvent, go ctx rs <$> produceNext)

resource ::
       (Reflex t, MonadWidget t m, MonadReader (GameState t) m)
    => ResourceConfig' t
    -> m (ResourceEffects t)
resource r@ResourceConfig {..}
    -- a pattern match matching on fields (like this one) forces the
    -- GameState to whnf by default. for early resources like wood and
    -- water, where we use the return value of this function to construct
    -- the initial GameState, we must not force it at all
 = do
    ~ctx@GameState {..} <- ask
    let rCount = inventoryDyn <&> \(Inventory i) -> M.findWithDefault 0 resourceType i
    rec (e, pb) <-
            divClass "resource-wrapper" $
            elDynKlass' "div" (resourceClass r rCount ctx) $ do
                elAttr "img" ("src" =: resourceImg) $ return ()
                elClass "span" "badge upper" $ text resourceName
                elClass "span" "badge lower" $ display rCount
                progressBar craftTime autoCraft rClick
        let canStartCrafting
                | craftable =
                    zipDynWith (\a m -> not a && m) (pbActive pb) (canMake r ctx)
                | otherwise = constDyn False
            rClick = gate (current canStartCrafting) (domEvent Click e)
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
                    [ fromIntegral <$> fmap (fromMaybe 1 resourceDenomination *) buyEvent
                    , negate . fromIntegral <$>
                      fforMaybe ingredientsUsed (M.lookup resourceType)
                    ]
    rhovered <- hoveredResourceRequirements r e
    let debitedResources = buyEvent <&> \cnt -> fmap (cnt *) resourceNeeds
    producedResources <-
        case resourceProduces of
            Just (n, rng) ->
                fmap Just $
                performEventAsync $
                buyEvent <&> \cnt cb ->
                    liftIO $ do
                        produced <- maybe (pure 1) randomRIO rng
                        cb (n, fromIntegral cnt * produced)
            Nothing -> pure Nothing
    return
        ResourceEffects
            { resourcesConsumed = debitedResources
            , ingredientsOfHoveredThing = rhovered
            , resourceTotal =
                  case producedResources of
                      Just e' -> ProducesOther e'
                      Nothing -> ProducesSelf $ (,) resourceType <$> rCountEv
            }
