module Control.Unsafe.Stasis(
                  Pod(..), FrozenPod(..),
                  stasis,version, put, get
                  ) where
import           Data.IORef(IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict  as M
import           System.IO.Unsafe (unsafePerformIO)

data Pod a = Stasis { val :: IORef (PodContent a), 
                      frozenVersions :: IORef (M.Map a Int)  
                    } deriving (Eq)

data PodContent a = Content {contentVal :: a, stasisVersion :: Int}
data FrozenPod a = FrozenPod { addr :: Pod a, frozenId :: Int }

stasis :: a -> Pod a
stasis v =    let p = unsafePerformIO $  newIORef M.empty
                  c = unsafePerformIO $  newIORef Content { contentVal = v, stasisVersion = 1}
              in
                 Stasis{val = c, frozenVersions = p }

put :: a -> Pod a -> Bool
put v p = unsafePerformIO $ 
                atomicModifyIORef' (val p) $ 
                \x -> let y = x{ contentVal = v, stasisVersion = stasisVersion x + 1}
                      in  (y,True)

version :: Pod a -> Int 
version v = stasisVersion (unsafePerformIO $ readIORef(val v))

get :: Pod a -> a
get pod = contentVal $ unsafePerformIO $ readIORef(val pod)