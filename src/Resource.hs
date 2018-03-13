{-# OPTIONS_GHC -ddump-splices #-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}

module Resource
    ( allResources
    , primResources
    , ResourceConfig(..)
    , Ingredients(..)
    , Resource(..)
    , Environment(..)
    ) where

import Resource.TH
import Resource.Types

genResource "resource.txt"
