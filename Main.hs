import Control.Monad.State
import Data.Function
import qualified Data.Map as Map
import Object
import World

r1 = room "one" "" [("n","two")] []

r2 = room "two" "" [("n","three")] []

r3 = room "three" "" [] []

w = giveObj r1 >> giveObj r2

test = do 
    giveObj r3
    takeObj "one"


loop room = do
    line <- liftIO $ do 
        putStrLn (name room)
        getLine 
    case line of 
        "stop" -> return ()
        name -> do 
            maybeRoom <- takeObj name
            case maybeRoom of
                Just newRoom -> do 
                    giveObj room
                    loop newRoom
                Nothing -> do
                    liftIO (putStrLn $
                        "can't go to " ++ name)
                    loop room 

main = do 
    runStateT (w >> loop r3) Map.empty
    return ()