import Control.Monad.State
import Data.Function
import qualified Data.Map as Map
import qualified Data.List as Ls 
import Object
import World

r1 = room "one" Nothing [("n","two")] []

r2 = room "two" Nothing [("n","three"), ("s", "one")] []

r3 :: Object
r3 = room "three" Nothing [("s", "two")] []

w = giveObj r1 >> giveObj r2

switch :: (Eq a) => a -> [(a, b)] -> b -> b
switch val branches else' = 
    maybe else' snd (Ls.find ((== val) . fst) branches)

loop room = do
    line <- liftIO $ do 
        putStrLn (_name room)
        getLine    
    switch line [
        ("stop", return ()),
        (_name room, do 
            liftIO $ putStrLn $ "Already in " ++ line
            loop room)]
        $ do 
            case _adj room >>= Map.lookup line of
                Just destName -> do
                    giveObj room
                    Just dest <- takeObj destName
                    loop dest
                Nothing -> do 
                    liftIO $ putStrLn $
                        "can't go to " ++ line
                    loop room            

main = runStateT (w >> loop r3) Map.empty