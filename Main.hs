import Control.Monad.State
import Data.Function
import qualified Data.Map as Map
import qualified Data.List as Ls 
import Object
import World

r1 = room "one" "" [("n","two")] []

r2 = room "two" "" [("n","three")] []

r3 = room "three" "" [] []

w = giveObj r1 >> giveObj r2

switch :: (Eq a) => a -> [(a, b)] -> b -> b
switch val branches def = 
    maybe def snd (Ls.find ((== val) . fst) branches)

loop room = do
    line <- liftIO $ do 
        putStrLn (name room)
        getLine    
    switch line [
        ("stop", return ()),
        (name room, do 
            liftIO $ putStrLn $ "Already in " ++ line
            loop room)]
        $ do 
            maybeRoom <- takeObj line
            case maybeRoom of
                Just newRoom -> do 
                    giveObj room
                    loop newRoom
                Nothing -> do
                    (liftIO $ putStrLn $
                        "can't go to " ++ line)
                    loop room

main = do 
    runStateT (w >> loop r3) Map.empty
    return ()