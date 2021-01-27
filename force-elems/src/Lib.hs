module Lib
    ( forceElems
    ) where

forceElems :: Traversable t => t a -> t a
forceElems = runId . traverse pure

data SId a = StrictId a | NonStrictId a

runId :: SId a -> a
runId (StrictId a) = a
runId (NonStrictId a) = a

instance Functor SId where
    fmap f fa = NonStrictId $ case fa of
        StrictId a -> f $! a
        NonStrictId a -> f a

instance Applicative SId where
    pure a = StrictId a
    ffa <*> fa = NonStrictId $ case (ffa, fa) of
        (NonStrictId f, StrictId a) -> f $! a
        (StrictId f, StrictId a) -> f $! a
        (NonStrictId f, NonStrictId a) -> f a
        (StrictId f, NonStrictId a) -> f a

