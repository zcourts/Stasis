{-|
     Stasis is a modified implementation of Multi-version concurrency control MVCC.
     The original purpose of Stasis was to provide a mechanism for having safe,
     \"mutable\" variables WITHOUT any locking whatsoever.

     How Stasis works

     Given an object a.
     When a is passed to a function it is wrapped in a 'Pod' which creates a
     "stasis" like environment for a.


     At any point, any function that received the 'Pod' can \"update\" it by
     'put'ting a new version of a.


     i.e. a itself is not updated, it is replaced. So what the 'Pod' does is 
     store the address of an a and an update changes the address the 'Pod' points 
     to, to another a.


     The address of a can only change to point to another value of a,
     i.e. it is type safe.


     At any time, a function who's received a 'Pod' can "freeze" it. In this case, 
     every time that function 'fetch'es a from the 'FrozenPod', the same version 
     that was frozen is always returned and at any time the function can also get 
     the latest version of a by 'get'ting the current non-frozen version.

     Internally b maintains a map of addresses to frozen IDs, when a function
     freezes a version of a it stores the address of a with a unique freeze id
     that is returned to the function. The function must then pass the freeze id
     each time it wants to get the frozen version of a for that id. This also means
     that a function can lock multiple versions of a.

     Originally the intention was to use pointers but IORefs works with atomic
     operations and achieves effectively the same thing.
     <http://hackage.haskell.org/package/base-4.5.1.0/docs/Foreign-StablePtr.html>
     <http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-IORef.html>
-}
module Control.Stasis(
                  -- * Primitives
                  -- $prims
                  Pod(..), FrozenPod(..),
                  -- * Operations
                  -- $ops
                  stasis, version, versionF, put, get, fetch, freeze, defrost
                  ) where
import           Data.IORef(IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict  as M
--import qualified Data.Unique as U (newUnique, hashUnique)
import Data.Maybe (fromMaybe)

--newStasisId :: IO Int
--newStasisId = do u <- U.newUnique; return (U.hashUnique u)

data Pod a = Stasis { val :: IORef (PodContent a), 
                      frozenVersions :: IORef (M.Map a Int)  
                    } deriving (Eq)

data PodContent a = Content {contentVal :: a, stasisVersion :: Int}
data FrozenPod a = FrozenPod { addr :: a, oPod :: Pod a, frozenAt :: Int}

stasis :: a -> IO (Pod a)
stasis v =    do p <- newIORef M.empty
                 c <- newIORef Content { contentVal = v, stasisVersion = 1}
                 return Stasis{val = c, frozenVersions = p }

put :: a -> Pod a -> IO Bool
put v p = atomicModifyIORef' (val p) $ 
                \x -> let y = x{ contentVal = v, stasisVersion = stasisVersion x + 1}
                      in  (y,True)

-- | Fetch the current version of a 'Pod'
version :: Pod a -> IO Int 
version v = do c <- readIORef(val v)
               return $ stasisVersion c

-- | Fetch the version the value was frozen at
versionF :: FrozenPod a -> Int 
versionF = frozenAt

-- | Get the value currently in 'Stasis'
get :: Pod a -> IO a
get pod = do c <- readIORef(val pod)
             return $ contentVal c

-- | Fetch the frozen value from the given 'FrozenPod'
fetch :: FrozenPod a -> a
fetch = addr

-- | Freeze the value of a at the current version
freeze :: Ord a => Pod a -> IO (FrozenPod a)
freeze p = do v <- get p
              cv <- version p
              let fp =  FrozenPod { addr = v, oPod = p, frozenAt = cv}
              m <- readIORef(frozenVersions p)
              let count = fromMaybe 0 (M.lookup v m) -- get current count or 0
              _ <- atomicModifyIORef' (frozenVersions p) $
              -- set count for v to current val + 1
               \x -> let r = M.insertWith (+) v (count + 1) x in (r,r)
              return fp

{-| Defrosting a 'FrozenPod' to a 'Pod' causes the value in stasis to be de-referenced.
Once this happens the value can be GC'd as would normally happen. Unless another function
has frozen the same version, in which case that function has to unfreeze the value as well
 -}
defrost ::Ord a => FrozenPod a -> IO (Pod a)
defrost fp = do let v = fetch fp
                let fv = frozenVersions (oPod fp)
                m <- readIORef fv
                let count = fromMaybe 0 (M.lookup v m)
                atomicModifyIORef' fv $
                 \x -> let r = M.insertWith (-) v (count + 1) x in (r,oPod fp)

{-$ops
     The type b supports 6 operations, 'stasis', 'put','get','fetch','freeze','defrost'.

     1. 'stasis' - creates a new 'Pod' which can be passed around in place of a

     2. 'put' - Put adds a new version of a to an existing 'Pod'

     3. 'get' - Gets the current version of a from a 'Pod'

     4. 'freeze' - Freezes a version of a in stasis, use 'defrost' on the returned
      'FrozenPod' to get the value back

     5. 'defrost' Returns the version of a for the given 'FrozenPod', i.e. it gets
     the value of a when it was frozen

     6. 'fetch' the same as 'defrost' but leaves a in a 'FrozenPod' which means it
     will not be garbage collected until it is 'defrost'ed
-}

{-$prims

'Stasis' has two primitives, a 'Pod' and a 'FrozenPod'.
A 'Pod' is 'created from any value a, once created, only new versions of a can be placed in the same pod.
A 'Pod' gets passed around in place of an ordinary value. 'Pod's can be frozen
to create a 'FrozenPod', when frozen the value of a is stays the same
-}
