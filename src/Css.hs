{-# Language TemplateHaskell #-}

module Css where

import Data.FileEmbed

css = $(embedFile "css/style.css")
