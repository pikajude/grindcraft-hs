{-# Language CPP #-}

#ifdef ghcjs_HOST_OS
module Natural where type Natural = Integer
#else
module Natural (module Numeric.Natural) where import Numeric.Natural
#endif
