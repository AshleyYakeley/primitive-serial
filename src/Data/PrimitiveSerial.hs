module Data.PrimitiveSerial where

import Control.Applicative
import Control.Monad
import Data.ByteString
import Data.ByteString.Internal
import Data.Word
import Foreign
import Prelude hiding (length)
import System.IO.Unsafe

newtype BSRead a = MkBSRead
    { unBSRead :: StrictByteString -> Int -> Maybe (Int, a)
    }

instance Functor BSRead where
    fmap ab (MkBSRead f) = MkBSRead $ \bs i -> fmap (fmap ab) $ f bs i

instance Applicative BSRead where
    pure a = MkBSRead $ \_ i -> Just (i, a)
    MkBSRead fab <*> MkBSRead fa =
        MkBSRead $ \bs i -> do
            (i', ab) <- fab bs i
            (i'', a) <- fa bs i'
            return (i'', ab a)

instance Alternative BSRead where
    MkBSRead fa <|> MkBSRead fb = MkBSRead $ \bs i -> fa bs i <|> fb bs i
    empty = MkBSRead $ \_ _ -> Nothing

instance Monad BSRead where
    MkBSRead f >>= q =
        MkBSRead $ \bs i -> do
            (i', a) <- f bs i
            unBSRead (q a) bs i'

instance MonadPlus BSRead

runWholeBSRead :: BSRead a -> StrictByteString -> Maybe a
runWholeBSRead (MkBSRead f) bs = do
    (i, a) <- f bs 0
    if i == length bs
        then return a
        else Nothing

bsRead :: BSRead Word8
bsRead =
    MkBSRead $ \bs i -> do
        w <- indexMaybe bs i
        return (succ i, w)

data Serialiser a = MkSerialiser
    { decode :: BSRead a
    , encode :: a -> StrictByteString
    }

nativeSerialiser ::
       forall a. Storable a
    => Serialiser a
nativeSerialiser = let
    typeSize :: Int
    typeSize = sizeOf (undefined :: a)
    decode :: BSRead a
    decode =
        MkBSRead $ \(BS fptr len) i -> let
            i' = i + typeSize
            in if i' <= len
                   then Just $ (i', unsafePerformIO $ withForeignPtr fptr $ \ptr -> peek $ plusPtr ptr i)
                   else Nothing
    encode :: a -> StrictByteString
    encode a = unsafePerformIO $ create typeSize $ \ptr -> poke (castPtr ptr) a
    in MkSerialiser {..}

class Storable a => FixedNumeric a where
    swapBytes :: a -> a

instance FixedNumeric Word8 where
    swapBytes = id
