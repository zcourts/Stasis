module Main where
import qualified Control.Stasis as S
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
        s <- S.stasis "abc"
        v <- S.get s
        ver <- S.version s
        print(show(ver) ++ ":" ++ v)
        --forever $ do
        _ <- S.put "hello "  s
        v2 <- S.get s
        ver2 <- S.version s
        print(show(ver2) ++ ":" ++ v2)
        threadDelay 10000