module World where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Control.Lens
import Object 

type World = Map.Map String Object

giveObj :: Object -> StateT World IO ()
giveObj obj = StateT $ \world -> 
    return ((), Map.insert (_name obj) obj world)

lookupObj :: String -> StateT World IO (Maybe Object)
lookupObj objName = StateT $ \world -> 
    return (Map.lookup objName world, world)

takeObj :: String -> StateT World IO (Maybe Object)
takeObj objName = StateT $ \world -> let
    obj = Map.lookup objName world
    world' = Map.delete objName world
    in return (obj, world')

takeObj' :: String -> StateT World IO (Maybe Object)
takeObj' objName = do
    world <- get
    let obj = Map.lookup objName world
        world' = Map.delete objName world
    put world'
    return obj

maybeAt :: (Functor f, Ord k) => 
    Maybe k -> 
    (Maybe b -> f (Maybe b)) -> Map.Map k b -> f (Map.Map k b)
maybeAt mKey wrap dict = 
    case (mKey, mKey >>= (dict Map.!?)) of        
        (Just key, Just val) -> 
            let replacer Nothing = Map.delete key dict
                replacer (Just val') = Map.insert key val' dict
            in fmap replacer (wrap $ Just val)
        _ -> dict <$ wrap Nothing

teleport :: String -> String -> World -> World
teleport objName destName world =
    let obj = Map.lookup objName world
        obj' = set (_Just . location . _Just) destName obj
        locName = obj ^. (_Just . location)
    in over 
        (at destName . _Just . contents . _Just) 
        (List.insert objName) $
        over
            (maybeAt locName . _Just . contents . _Just)
            (List.delete objName) 
            (world & at objName .~ obj')

w = Map.fromList [
    ("ball", Object "ball" (Just "one") Nothing Nothing),
    ("one", room "one" Nothing [("n","two")] ["ball"]),
    ("two", room "two" Nothing [("n","three"), ("s", "one")] []),
    ("three", room "three" Nothing [("s", "two")] [])
    ]