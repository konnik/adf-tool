module Adf.Util where

unwrap :: (Show err) => Either err a -> a
unwrap x = case x of
    Right a -> a
    Left err -> error $ "Unwrap failed: " ++ show err
