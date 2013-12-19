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

import qualified Data.Array.Repa.Eval           as R

import Data.Bits ((.&.))


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

        aimg <- either (error . show) id `fmap` A.readImageFromBMP fileIn
        let myimg = aimg
            Right myimg2 = (decomposeRGBA (A.use myimg) >>= composeRGBA) 

        A.writeImageToBMP fileOut (ParseArgs.run backend myimg2)
        return ()


decomposeRGBA chan = do 
                       let dchan = A.map A.unpackRGBA32 chan
                           (r,g,b,a) = A.unzip4 dchan
                       return (r,g,b,a)

composeRGBA (r,g,b,a) = do
                     let rgba = A.map A.packRGBA32 (A.zip4 r g b a)
                     return $ rgba

