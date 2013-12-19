{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Applicative

import qualified Config                        as Cfg
import qualified ParseArgs                     as ParseArgs

import           Prelude                       as P
import qualified Data.Label                    as Label
import           Criterion.Main                ( defaultMainWith, bgroup, bench, whnf )
import qualified System.Exit                   as Exit
import qualified System.Environment            as Env

import           Data.Monoid                   (mempty, Monoid)
import qualified Data.Array.Accelerate         as A
import           Data.Array.Accelerate         (Acc, Exp, (:.)(..))
import qualified Data.Array.Accelerate.IO      as A
import qualified Data.Array.Repa.IO.BMP        as R
import qualified Data.Array.Repa.Repr.Unboxed  as R
import qualified Data.Array.Repa               as R
import qualified Data.Map                      as Map
import           Data.Map                      (Map)
import qualified Data.Array.Repa.IO.DevIL      as DevIL

import           System.TimeIt                 (timeIt)

import           Data.Array.Repa.Eval          (Target)
import           Data.Word                     (Word8)

import qualified Graphics.Raster.Image    as Image
import qualified Graphics.Raster.Image.IO as Image
import           Graphics.Raster.Image    (Image)
import qualified Graphics.Raster.Channel  as Channel
import           Graphics.Raster.Channel  (Channel)

import qualified Data.Array.Repa.Eval           as R

import Data.Bits ((.&.))


import Control.Monad.Trans.Either (runEitherT, hoistEither)


imgtest :: Image A.Word32 -> Either Image.Error (Image A.Word32)
imgtest img = Image.composeRGBA =<< Image.decomposeRGBA img


main :: IO ()
main
  = do
        argv                    <- Env.getArgs
        (conf, cconf, nops)     <- ParseArgs.parseArgs Cfg.configHelp Cfg.configBackend Cfg.options Cfg.defaults Cfg.header Cfg.footer argv
        (fileIn, fileOut)       <- case nops of
          (i:o:_) -> return (i,o)
          _       -> ParseArgs.parseArgs Cfg.configHelp Cfg.configBackend Cfg.options Cfg.defaults Cfg.header Cfg.footer ("--help":argv)
                  >> Exit.exitSuccess

        let backend     = Label.get Cfg.configBackend conf

        img2 <- either (\_ -> mempty) id `fmap` Image.readImageFromBMP' fileIn
        let img3 = imgtest img2

        --print "---"
        --print img2
        --print "---"
        --print $ fmap (Image.compute (ParseArgs.run backend)) $ Image.decomposeRGBA img2
        print "---"
        print $ fmap (Image.compute (ParseArgs.run backend)) $ (Image.composeRGBA =<< Image.decomposeRGBA img2)

        case img3 of 
            Left  err -> print err
            Right val -> do Image.writeImageToBMP' (ParseArgs.run backend) fileOut val
                            return ()

       