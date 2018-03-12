{-# Language TypeApplications #-}
{-# Language FlexibleContexts #-}

module Resource.Parser where

import Control.Applicative
import Data.Maybe
import Debug.Trace
import Text.Parser.LookAhead
import Text.Trifecta
import Data.List.NonEmpty
import Text.Trifecta.Indentation

nameTok = liftA2 (:) letter (many alphaNum)

resourcesP = resourceP `sepBy` some nl <* eof

resourceP = do
    name <- nameTok
    as <- nl *> localIndentation Gt attrs
    return (name, as)

nl = newline *> localIndentation Any (many (char ' '))

attrs =
    many $
    absoluteIndentation $
    choice
        [ aName <* nl
        , aImg <* nl
        , aDenom <* nl
        , aProd <* nl
        , RHidden <$ string "hidden" <* nl
        , RNocraft <$ string "nocraft" <* nl
        , RPrim <$ string "prim" <* nl
        , aTime
        , aWants
        ]
  where
    aDenom = fmap RDenom $ string "denom" *> some (char ' ') *> decimal

aName =
    fmap RName $
    string "name" *> some (char ' ') *> between (char '"') (char '"') (many (notChar '"'))

aImg =
    fmap RImg $
    string "img" *> some (char ' ') *> between (char '"') (char '"') (many (notChar '"'))

aWants = do
    w <- string "wants" <|> string "needs"
    nl
    fmap
        (if w == "wants"
             then RWants
             else RNeeds) $
        localIndentation Gt $ many $ absoluteIndentation $ req <* nl
  where
    req = do
        n <- nameTok
        many (char ' ')
        x <- lookAhead anyChar
        names <-
            if x == '|'
                then (:|) n <$>
                     some
                         (try $ many (char ' ') *> char '|' *> many (char ' ') *> nameTok)
                else pure $ n :| []
        (,) names <$> (many (char ' ') *> optional decimal)

aTime =
    fmap RTime $ do
        string "time"
        nl
        localIndentation Gt $ do
            d <-
                absoluteIndentation $ do
                    string "default"
                    some (char ' ')
                    decimal <* nl
            (,) d <$> many (absoluteIndentation $ timeAttr <* nl)
  where
    timeAttr = do
        c <- nameTok
        some (char ' ')
        n <- read <$> some (digit <|> char '.')
        return $ (c, n)

aProd =
    fmap RProduces $ do
        string "produces"
        some (char ' ')
        rsrc <- nameTok
        many (char ' ')
        r <- lookAhead anyChar
        if r == '\n'
            then return (rsrc, Nothing)
            else do
                r1 <- decimal
                many (char ' ')
                char '-'
                many (char ' ')
                r2 <- decimal
                return (rsrc, Just (r1, r2))

data RField
    = RName String
    | RImg String
    | RDenom Integer
    | RTime (Integer, [(String, Double)])
    | RNeeds [(NonEmpty String, Maybe Integer)]
    | RWants [(NonEmpty String, Maybe Integer)]
    | RProduces (String, Maybe (Integer, Integer))
    | RHidden
    | RNocraft
    | RPrim
    deriving (Show, Eq)

parser =
    runIndentationParserT @Char resourcesP (mkIndentationState 0 infIndentation True Any)
