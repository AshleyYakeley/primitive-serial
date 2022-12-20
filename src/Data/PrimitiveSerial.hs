module Data.PrimitiveSerial where

import Control.Applicative
import Control.Monad
import Data.ByteString
import Data.ByteString.Internal
import Data.Word
import Foreign
import Prelude hiding (drop, length)
import System.Endian
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

runBSRead :: BSRead a -> StrictByteString -> Maybe (StrictByteString, a)
runBSRead (MkBSRead f) bs = do
    (i, a) <- f bs 0
    if i <= length bs
        then return (drop i bs, a)
        else Nothing

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

decodeNative ::
       forall a. Storable a
    => BSRead a
decodeNative = let
    typeSize :: Int
    typeSize = sizeOf (undefined :: a)
    in MkBSRead $ \(BS fptr len) i -> let
           i' = i + typeSize
           in if i' <= len
                  then Just $ (i', unsafePerformIO $ withForeignPtr fptr $ \ptr -> peek $ plusPtr ptr i)
                  else Nothing

encodeNative ::
       forall a. Storable a
    => a
    -> StrictByteString
encodeNative a = let
    typeSize :: Int
    typeSize = sizeOf (undefined :: a)
    in unsafePerformIO $ create typeSize $ \ptr -> poke (castPtr ptr) a

class Storable a => FixedNumeric a where
    nativeToLittleEndian :: a -> a
    nativeToBigEndian :: a -> a

instance FixedNumeric Word8 where
    nativeToLittleEndian = id
    nativeToBigEndian = id

instance FixedNumeric Word16 where
    nativeToLittleEndian = fromLE16
    nativeToBigEndian = fromBE16

instance FixedNumeric Word32 where
    nativeToLittleEndian = fromLE32
    nativeToBigEndian = fromBE32

instance FixedNumeric Word64 where
    nativeToLittleEndian = fromLE64
    nativeToBigEndian = fromBE64

decodeLittleEndian ::
       forall a. FixedNumeric a
    => BSRead a
decodeLittleEndian = fmap nativeToLittleEndian $ decodeNative

encodeLittleEndian ::
       forall a. FixedNumeric a
    => a
    -> StrictByteString
encodeLittleEndian = encodeNative . nativeToLittleEndian

decodeBigEndian ::
       forall a. FixedNumeric a
    => BSRead a
decodeBigEndian = fmap nativeToBigEndian $ decodeNative

encodeBigEndian ::
       forall a. FixedNumeric a
    => a
    -> StrictByteString
encodeBigEndian = encodeNative . nativeToBigEndian
