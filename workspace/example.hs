module Main where

import GDAL

main :: IO ()
main = do
  putStrLn "testing hgdal"
  gDALAllRegister
  pure ()
