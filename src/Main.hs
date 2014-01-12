module Main where
import qualified Control.Stasis as S

main :: IO ()
main = do
        -- put the value "abc" into stasis
        s <- S.stasis "abc"
        v <- S.get s
        ver <- S.version s
        print(show ver ++ ":" ++ v) -- version 1, abc
        --forever $ do
        -- replace "abc" with hello
        _ <- S.put "hello "  s
        v2 <- S.get s
        ver2 <- S.version s
        print(show ver2 ++ ":" ++ v2) -- version 2, hello
        --freeze version two
        frozenS <- S.freeze s
        _ <- S.put " replaced hello" s
        v3 <- S.get s
        ver3 <- S.version s
        -- notice action on frozen version is pure, always returns same thing
        let v2Frozen = S.fetch frozenS
        let ver2F = S.versionF frozenS
        print(show ver3 ++ ":" ++ v3)
        -- 2:hello because these values were frozen at version 2
        print(show ver2F ++ ":" ++ v2Frozen) -- 
