{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Tim as Tim
import qualified Data.Text as T

lengths = [10,100,1000,10000,1000000]

descending_int :: Int -> V.Vector Int
descending_int len =  V.enumFromStepN len (-1) len

ascending_int :: Int -> V.Vector Int
ascending_int len = V.enumFromN 0 len

all_equal_int :: Int -> V.Vector Int
all_equal_int len = V.replicate len 667

pseudo_ascending_string :: Int -> V.Vector T.Text
pseudo_ascending_string len = V.generate len (T.pack . show)

main = defaultMain (concatMap (\ len -> [
                                env (return $ descending_int len) 
                                        (\ arr -> bench ("DESCENDING_INT_" ++ show len) $ nfIO $ Tim.sort =<< V.unsafeThaw arr)
                               ,env (return $ ascending_int len) 
                                        (\ arr -> bench ("ASCENDING_INT_" ++ show len) $ nfIO $ Tim.sort =<< V.unsafeThaw arr)
                               ,env (return $ all_equal_int len)
                                    (\ arr -> bench ("ALL_EQUAL_INT_" ++ show len) $ nfIO $ Tim.sort =<< V.unsafeThaw arr)
                               , env (return $ pseudo_ascending_string len)
                                         (\ arr -> bench ("PSEUDO_ASCENDING_STRING" ++ show len) $ nfIO $ Tim.sort =<< V.unsafeThaw arr)
                               ]) lengths)

