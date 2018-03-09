{-# Language TemplateHaskell #-}

module Resource.TH
    ( r
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Resource.Types
import Text.Parser.LookAhead
import Text.Trifecta

r :: QuasiQuoter
r =
    QuasiQuoter
        { quoteDec =
              \s -> do
                  qb <- newName "qb"
                  let exprs =
                          parseString (spaces >> (rsrcP qb `sepBy` some newline)) mempty s
                  case unzip <$> exprs of
                      Success (cons, a) ->
                          sequence
                              [ funD
                                    (mkName "allResources")
                                    [clause [varP qb] (normalB (listE a)) []]
                              , dataD
                                    (cxt [])
                                    (mkName "Resource")
                                    []
                                    Nothing
                                    (map mkCon cons)
                                    [ derivClause
                                          (Just StockStrategy)
                                          [ [t|Ord|]
                                          , [t|Eq|]
                                          , [t|Show|]
                                          , [t|Bounded|]
                                          , [t|Enum|]
                                          ]
                                    ]
                              ]
                      Failure m -> error $ show m
        }
  where
    mkCon str = normalC (mkName str) []

rsrcP inventoryVar = do
    w <- some alphaNum
    newline
    fs <- fields
    return $
        ( w
        , recUpdE [|defaultResourceConfig|] $
          map (fieldToConQ inventoryVar) (RType w : fs))

fields = go =<< lookAhead (some space)
  where
    go padding = do
        string padding
        fieldName <- some alphaNum
        many $ satisfy (== ' ')
        fieldVal <-
            case fieldName of
                "name" -> (RName <$> imgField) <* newline
                "img" -> (Img <$> imgField) <* newline
                "denom" -> (Denom <$> decimal) <* newline
                "time" -> newline *> (uncurry CraftTimes <$> timeField)
                "needs" -> newline *> (Needs <$> needsField)
                "wants" -> newline *> (Wants <$> needsField)
                "hidden" -> newline $> Hidden
                "nocraft" -> newline $> NoCraft
                "produces" -> (uncurry Produces <$> produceField) <* newline
                x -> error $ show x
        (:) fieldVal . fromMaybe [] <$> optional (go padding)
    imgField = between (char '"') (char '"') $ some (satisfy (/= '"'))

produceField = do
    w <- some alphaNum
    spaces
    range <- optional $ do
        r1 <- decimal
        spaces
        char '-'
        spaces
        r2 <- decimal
        return (r1, r2)
    return (w, range)

timeField = go =<< lookAhead (some space)
  where
    go padding = do
        string padding
        needsName <- some alphaNum
        spaces
        craftTime <- read <$> some (digit <|> char '.')
        newline
        case needsName of
            "default" ->
                first (const craftTime) . fromMaybe (craftTime, []) <$>
                optional (go padding)
            _ ->
                second ((:) (needsName, craftTime)) . fromMaybe (0, []) <$>
                optional (go padding)

needsField = go =<< lookAhead (some space)
  where
    go padding =
        some $ do
            string padding
            needsName <- some alphaNum
            spaces
            craftTime <- decimal
            newline
            return (needsName, craftTime)

data FieldVal
    = Img String
    | CraftTimes Double
                 [(String, Double)]
    | Needs [(String, Integer)]
    | Wants [(String, Integer)]
    | Produces String (Maybe (Integer, Integer))
    | RType String
    | RName String
    | Denom Integer
    | Hidden
    | NoCraft
    deriving (Show)

fieldToConQ _ (Img s) = (,) 'resourceImg <$> [|T.pack $(stringE s)|]
fieldToConQ _ (RType s) = (,) 'resourceType <$> conE (mkName s)
fieldToConQ _ (RName s) = (,) 'resourceName <$> [|T.pack $(stringE s)|]
fieldToConQ ivar (CraftTimes n cs) =
    (,) 'craftTime <$>
    [|Just (craftTimesOf n (M.fromList $(listE (map ctime cs))) $(varE ivar))|]
  where
    ctime (s, d) = [|($(conE (mkName s)), d)|]
fieldToConQ _ (Needs rs) =
    (,) 'resourceNeeds <$> [|Just (Ingredients (M.fromList $(listE (map rt rs))))|]
  where
    rt (s, d) = [|($(conE (mkName s)), d)|]
fieldToConQ _ (Wants rs) =
    (,) 'resourceWants <$> [|Just (Ingredients (M.fromList $(listE (map rt rs))))|]
  where
    rt (s, d) = [|($(conE (mkName s)), d)|]
fieldToConQ _ (Denom n) = (,) 'resourceDenomination <$> [|Just n|]
fieldToConQ _ Hidden = (,) 'ingredientsControlVisibility <$> [|True|]
fieldToConQ _ NoCraft = (,) 'craftable <$> [|False|]
fieldToConQ _ (Produces n r) = (,) 'resourceProduces <$> [|Just ($(conE (mkName n)), r)|]
fieldToConQ _ x = error $ show x
