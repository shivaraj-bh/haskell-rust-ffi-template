module Main where

import Foreign.C.String

-- | The `hello` function exported by the `hello_rust` library.
foreign import ccall "hello" hello_rust :: IO CString

{- |
  Convert a C string to a Haskell string.
-}
hello :: IO String
hello = hello_rust >>= peekCString

main :: IO ()
main = hello >>= putStrLn
