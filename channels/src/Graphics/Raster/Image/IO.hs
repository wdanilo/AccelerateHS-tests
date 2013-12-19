{-# LANGUAGE ViewPatterns #-}

module Graphics.Raster.Image.IO where

import           Prelude               hiding(map)

import qualified Data.Array.Accelerate         as A
import           Data.Array.Accelerate         (Acc, Exp, (:.)(..))
import qualified Data.Array.Accelerate.IO      as A
import           Data.Bits                     ((.&.))
import           Data.Monoid                   (mempty, Monoid)
import qualified Codec.BMP                     as BMP
import           Control.Applicative
import           Control.Monad.IO.Class        (MonadIO, liftIO)

import qualified Graphics.Raster.Image         as Image
import           Graphics.Raster.Image         (Image)
import qualified Graphics.Raster.Channel       as Channel
import           Graphics.Raster.Channel       (Channel)
import           Control.Monad.Trans.Either    (runEitherT, hoistEither)


readImageFromBMP' :: MonadIO m => FilePath -> m (Either BMP.Error (Image A.Word32))
readImageFromBMP' file = liftIO(fmap mkChan <$> A.readImageFromBMP file) where
    mkChan chdata = Image.insert "rgba" (Channel.Raw chdata) mempty


writeImageToBMP' :: MonadIO m => Channel.Backend A.Word32 -> FilePath -> Image A.Word32 -> m (Either Image.Error ())
writeImageToBMP' backend file img = runEitherT $ do 
    chan <- hoistEither $ Image.lookup' "rgba" img
    let Channel.Raw mdata = Channel.compute backend chan
    liftIO $ A.writeImageToBMP file mdata


decomposeRGBA :: Image A.Word32 -> Either Image.Error (Image A.Word8)
decomposeRGBA img = do chan <- Image.lookup' "rgba" img
                       let dchan = Channel.map unpackRGBA32 chan
                           (r,g,b,a) = Channel.unzip4 dchan
                           outimg    = Image.insert "r" r
                                     $ Image.insert "g" g
                                     $ Image.insert "b" b
                                     $ Image.insert "a" a
                                     $ mempty
                       return outimg


composeRGBA :: Image A.Word8 -> Either Image.Error (Image A.Word32)
composeRGBA img = do r <- Image.lookup' "r" img
                     g <- Image.lookup' "g" img
                     b <- Image.lookup' "b" img
                     a <- Image.lookup' "a" img
                     
                     let rgba = Channel.map packRGBA32 (Channel.zip4 r g b a)
                         --rgba = trace(">> r: " ++ show (Channel.compute Interp.run r))
                         --     $ trace(">> g: " ++ show (Channel.compute Interp.run g))
                         --     $ trace(">> b: " ++ show (Channel.compute Interp.run b))
                         --     $ trace(">> a: " ++ show (Channel.compute Interp.run a))
                         --     $ rgba'
                     return $ Image.insert "rgba" rgba mempty


packRGBA32 :: Exp (A.Word8, A.Word8, A.Word8, A.Word8) -> Exp A.RGBA32
packRGBA32 rgba = r + g + b + a
  where (r', g', b', a')  = A.unlift rgba
        r                 = A.fromIntegral r'
        g                 = (A.fromIntegral g') * 0x100
        b                 = (A.fromIntegral b') * 0x10000
        a                 = (A.fromIntegral a') * 0x1000000


unpackRGBA32 :: Exp A.RGBA32 -> Exp (A.Word8, A.Word8, A.Word8, A.Word8)
unpackRGBA32 rgba =
  let r = A.fromIntegral $ rgba                   .&. 0xFF
      g = A.fromIntegral $ (rgba `div` 0x100)     .&. 0xFF
      b = A.fromIntegral $ (rgba `div` 0x10000)   .&. 0xFF
      a = A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF
  in
  A.lift (r, g, b, a)
