import Control.Monad.State
import Data.Function
import qualified Data.Map as Map
import qualified Data.List as Ls 
import Object
import World

r1 = room "one" "" [("n","two")] []

r2 = room "two" "" [("n","three"), ("s", "one")] []

r3 = room "three" "" [("s", "two")] []

w = giveObj r1 >> giveObj r2

switch :: (Eq a) => a -> [(a, b)] -> b -> b
switch val branches otherwise = 
    maybe otherwise snd (Ls.find ((== val) . fst) branches)

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
            case adj room >>= Map.lookup line of
                Just destName -> do
                    giveObj room
                    Just dest <- lookupObj destName
                    loop dest
                Nothing -> do 
                    (liftIO $ putStrLn $
                        "can't go to " ++ line)
                    loop room            

main = runStateT (w >> loop r3) Map.empty