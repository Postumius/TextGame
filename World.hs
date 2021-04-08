module World where

import Control.Monad.State
import qualified Data.Map as Map
import Object 

type World = Map.Map String Object

giveObj :: Object -> StateT World IO ()
giveObj obj = StateT $ \world -> 
    return ((), Map.insert (name obj) obj world)

takeObj :: String -> StateT World IO (Maybe Object)
takeObj name = StateT $ \world -> let
    obj = Map.lookup name world
    world' = Map.delete name world
    in return (obj, world')

takeObj' :: String -> StateT World IO (Maybe Object)
takeObj' name = do
    world <- get
    let obj = Map.lookup name world
        world' = Map.delete name world
    put world'
    return obj