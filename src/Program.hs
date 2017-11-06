module Main where
import System.IO
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad(join, void)
import Data.List(sortBy)
import Data.Function(on)

main :: IO ()
main = void . join $ (runStateT . sequence_ . (fmap receivePacket) . lines <$> getContents) <*> pure Map.empty

type PacketId = Int
type PacketBuffer = Map.Map PacketId [(Int, String)]
type RawPacket = String

recievePacket' :: (Monad m) => RawPacket -> PacketBuffer -> m (String, PacketBuffer)
recievePacket' raw buffer = let pid:pno:plen:_ = fmap read $ take 3 $ words raw
                                p = (pno, raw) : Map.findWithDefault [] pid buffer
                                np = Map.insert pid p buffer
                            in return $ if (length p == plen - 1) then (unlines $ snd <$> sortBy (compare `on` fst) p, Map.delete pid np) else ("", np)

recievePacket :: RawPacket -> StateT PacketBuffer IO ()
recievePacket raw = (StateT $ recievePacket' raw) >>= lift.putStr
