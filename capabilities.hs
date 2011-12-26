{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Capabilties (
runCap,
IOtoCap,
makeCap,
) where

newtype Cap a = Cap { runCap :: IO a }

{-
IO a -> Cap a
(a -> IO b) -> (a -> Cap b)
(a -> (b -> IO c)) -> (a -> (b -> Cap c))
-}

class IOtoCap a b | a -> b where
    unsafeMakeFunCap :: a -> b
    
instance IOtoCap (IO a) (Cap a) where
    unsafeMakeFunCap = Cap
    
instance (IOtoCap a b) => IOtoCap (c -> a) (c -> b) where
    -- The argument to unsafeMakeFunCap should be an (c -> a)
    -- The argument to the lambda should be of the type c mentioned in the previous comment
    unsafeMakeFunCap cToA = \c -> unsafeMakeFunCap (cToA c)
    
makeCap :: (IOtoCap a b) => a -> IO b
makeCap = return . unsafeMakeFunCap

instance Monad Cap where
    return = Cap . return
    x >>= f = error "Tomorrow"

--instance Functor Cap where
    --fmap 
