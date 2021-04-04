module World where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Function
import qualified Object as Obj

data Room = Room { 
    name::String, 
    contents::[String]
    } deriving (Show)

type World = Map.Map String Room

giveRoom :: Room -> State World ()
giveRoom room = state $ \world -> 
    ((), Map.insert (name room) room world)

takeRoom :: String -> State World (Maybe Room)
takeRoom name = state $ \world -> let
    room = Map.lookup name world
    world' = Map.delete name world
    in (room, world')

ball = Obj.Object "ball" "r1" Nothing

r1 = Room "one" ["ball"]

r2 = Room "two" []

r3 = Room "three" []

w = Map.empty & (runState $ giveRoom r1 >> giveRoom r2) & snd

test = do 
    giveRoom r3
    takeRoom "one"