{-# OPTIONS_GHC -ddump-splices #-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}

module Resource
    ( allResources
    , ResourceConfig(..)
    , Ingredients(..)
    , Resource(..)
    ) where

import Resource.TH
import Resource.Types

genResource "resource.txt"
