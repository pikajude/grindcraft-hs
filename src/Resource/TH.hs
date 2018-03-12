{-# Language CPP #-}
{-# Language DeriveLift #-}
{-# Language StandaloneDeriving #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}

module Resource.TH
    ( genResource
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader hiding (lift)
import Data.Functor
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Reflex.Class (Dynamic, Reflex)
import Resource.Parser (RField(..), parser)
import Resource.Types
import Text.Parser.LookAhead
import Text.Trifecta

#ifdef ghcjs_HOST_OS
type Natural = Integer
#else
import Numeric.Natural
#endif

deriving instance Lift a => Lift (NonEmpty a)

#if !MIN_VERSION_template_haskell(2,12,0)
dClause _ x = cxt x
#else
dClause a b = [derivClause a b]
#endif

genResource :: String -> DecsQ
genResource s = do
    result <- runIO $ parseFromFileEx parser s
    case result of
        Success (unprims', _) -> do
            let prims = filter (\(_, x) -> RPrim `elem` x) unprims'
                unprims =
                    map
                        (\(c, x) ->
                             if RPrim `elem` x
                                 then (c, RNocraft : x)
                                 else (c, x))
                        unprims'
            n <- newName "arg"
            sequence
                [ dataD
                      (cxt [])
                      (mkName "Resource")
                      []
                      Nothing
                      (map (mkRCon . fst) unprims)
                      (dClause
                           Nothing
                           [[t|Show|], [t|Ord|], [t|Eq|], [t|Bounded|], [t|Enum|]])
                , sigD
                      (mkName "allResources")
                      [t|forall t. Reflex t =>
                                       Dynamic t (Map $(conT $ mkName "Resource") Natural) -> [ResourceConfig $(conT $
                                                                                                                mkName
                                                                                                                    "Resource") t]|]
                , funD
                      (mkName "allResources")
                      [clause [varP n] (normalB (listE $ map (mkResource n) unprims)) []]
                , sigD
                      (mkName "primResources")
                      [t|forall t. Reflex t =>
                                       Dynamic t (Map $(conT $ mkName "Resource") Natural) -> [ResourceConfig $(conT $
                                                                                                                mkName
                                                                                                                    "Resource") t]|]
                , funD
                      (mkName "primResources")
                      [clause [varP n] (normalB (listE $ map (mkResource n) prims)) []]
                ]
        Failure xs -> error $ show xs
  where
    mkRCon s = normalC (mkName s) []

mkResource n (s, fields) =
    recUpdE
        [|defaultResourceConfig|]
        (pure ('resourceType, ConE (rName s)) : map (mkUpd n) fields)

mkUpd _ (RName s) = strength ('resourceName, [|T.pack $(stringE s)|])
mkUpd _ (RImg s) = strength ('resourceImg, [|T.pack $(stringE s)|])
mkUpd p (RTime (def, xs)) =
    strength
        ( 'craftTime
        , [|Just (craftTimesOf def (M.fromList $(listE $ map timePair xs)) $(varE p))|])
  where
    timePair (n, x) = [|($(rCon n), x)|]
mkUpd _ (RNeeds ps) = strength ('resourceNeeds, [|M.fromList $(liftNeeds ps)|])
mkUpd _ (RWants ps) = strength ('resourceWants, liftWants ps)
mkUpd _ (RProduces (x, y)) = strength ('resourceProduces, [|Just ($(rCon x), y)|])
mkUpd _ (RDenom n) = strength ('resourceDenomination, [|Just n|])
mkUpd _ RHidden = strength ('ingredientsControlVisibility, [|True|])
mkUpd _ RNocraft = strength ('craftable, [|False|])
mkUpd _ RPrim = strength ('resourcePrim, [|True|])

liftWants = listE . map (\(p :| ps, _) -> [|S.fromList $(listE $ map rCon (p : ps))|])

liftNeeds =
    listE . map (\(p :| [], n) -> [|($(rCon p), $(litE $ integerL $ fromMaybe 1 n))|])

rName s = mkName ("Resource." ++ s)

rCon = conE . rName

strength = uncurry (fmap . (,))
