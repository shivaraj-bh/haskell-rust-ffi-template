{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Main where

import Foreign.C.String (peekCString)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr, nullPtr)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Foreign.C.Types (CChar)
import Data.Word (Word8)
import Control.Exception (try)
import Foreign.Ptr (castPtr)
import qualified Data.ByteString.Base64 as B64

-- FFI Imports
-- | Encrypt data: takes key bytes, key length, plaintext bytes, plaintext length
foreign import ccall "encrypt_data" 
  encrypt_data_c :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO (Ptr CChar)

-- | Decrypt data: takes key bytes, key length, encrypted bytes, encrypted length
foreign import ccall "decrypt_data" 
  decrypt_data_c :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO (Ptr CChar)

-- | Free C string allocated by Rust
foreign import ccall "free_string" 
  free_string_c :: Ptr CChar -> IO ()

-- | Safe wrapper for encrypt_data
-- Returns encrypted data (base64 encoded for transmission) or error message
encrypt_haskell :: ByteString -> ByteString -> IO (Either String ByteString)
encrypt_haskell keyBytes plaintextBytes = do
  let keyLen = BS.length keyBytes
      plaintextLen = BS.length plaintextBytes
  
  -- Use withByteStrings to safely pass ByteStrings to C
  result <- BS.useAsCString keyBytes $ \keyPtr ->
    BS.useAsCString plaintextBytes $ \plaintextPtr -> do
      resultPtr <- encrypt_data_c (castCStringToPtr keyPtr) (fromIntegral keyLen) (castCStringToPtr plaintextPtr) (fromIntegral plaintextLen)
      
      if resultPtr == nullPtr then return Nothing
      else do
        -- Read the encrypted data from C string (base64 encoded)
        encryptedStr <- peekCString resultPtr
        free_string_c resultPtr
        return $ Just (BS.pack (map (fromIntegral . fromEnum) encryptedStr))
  
  case result of
    Nothing -> return $ Left "Encryption failed"
    Just encryptedB64 -> return $ Right encryptedB64

-- | Safe wrapper for decrypt_data
-- Returns decrypted plaintext or error message
decrypt_haskell :: ByteString -> ByteString -> IO (Either String ByteString)
decrypt_haskell keyBytes encryptedRawBytes = do
  let keyLen = BS.length keyBytes
      encryptedLen = BS.length encryptedRawBytes
  
  result <- BS.useAsCString keyBytes $ \keyPtr ->
    BS.useAsCStringLen encryptedRawBytes $ \(encryptedPtr, _) -> do
      resultPtr <- decrypt_data_c (castCStringToPtr keyPtr) (fromIntegral keyLen) (castPtr encryptedPtr) (fromIntegral encryptedLen)
      
      if resultPtr == nullPtr then return Nothing
      else do
        -- Read the decrypted plaintext from C string
        decryptedStr <- peekCString resultPtr
        free_string_c resultPtr
        return $ Just (BS.pack (map (fromIntegral . fromEnum) decryptedStr))
  
  case result of
    Nothing -> return $ Left "Decryption failed"
    Just decrypted -> return $ Right decrypted

-- Helper function to read file as ByteString
readFileBS :: FilePath -> IO ByteString
readFileBS = BS.readFile

castCStringToPtr :: Ptr CChar -> Ptr Word8
castCStringToPtr = castPtr

-- | Test encrypt/decrypt round trip
testEncryptDecrypt :: ByteString -> ByteString -> ByteString -> String -> IO Bool
testEncryptDecrypt pubKey privKey plaintext testName = do
  putStrLn $ "\n--- Test: " ++ testName ++ " (plaintext size: " ++ show (BS.length plaintext) ++ " bytes) ---"
  encResult <- encrypt_haskell pubKey plaintext
  
  case encResult of
    Left err -> do
      putStrLn $ "Encryption error: " ++ err
      return False
    Right encryptedB64 -> do
      putStrLn $ "Encrypted (base64): " ++ show (BS.length encryptedB64) ++ " bytes"
      
      -- Decrypt expects raw binary, so decode the base64 first
      case B64.decode encryptedB64 of
        Left decErr -> do
          putStrLn $ "Base64 decode error: " ++ decErr
          return False
        Right encryptedRaw -> do
          decResult <- decrypt_haskell privKey encryptedRaw
          
          case decResult of
            Left err -> do
              putStrLn $ "Decryption error: " ++ err
              return False
            Right decrypted -> do
              if plaintext == decrypted
                then do
                  putStrLn "✓ Round-trip successful (plaintext matches)"
                  return True
                else do
                  putStrLn "✗ Round-trip FAILED (plaintext mismatch)"
                  putStrLn $ "Expected (" ++ show (BS.length plaintext) ++ " bytes): " ++ take 50 (show plaintext) ++ "..."
                  putStrLn $ "Got (" ++ show (BS.length decrypted) ++ " bytes): " ++ take 50 (show decrypted) ++ "..."
                  return False

-- | Test main function
main :: IO ()
main = do
  putStrLn "\n=====================================\nhs<>rs PGP\n=====================================\n"

  pubKeyResult <- try (readFileBS "../rust/src/recipient_public.asc") :: IO (Either IOError ByteString)
  privKeyResult <- try (readFileBS "../rust/src/private_key.asc") :: IO (Either IOError ByteString)
  
  case (pubKeyResult, privKeyResult) of
    (Left err, _) -> putStrLn $ "Error reading public key: " ++ show err
    (_, Left err) -> putStrLn $ "Error reading private key: " ++ show err
    (Right pubKey, Right privKey) -> do      
      -- Read plaintext from file
      plaintextFileResult <- try (readFileBS "../rust/plain.txt") :: IO (Either IOError ByteString)
      
      case plaintextFileResult of
        Left err -> putStrLn $ "Error reading plaintext file: " ++ show err
        Right plaintext -> do
          -- Encrypt the file content
          putStrLn "\nEncrypting plaintext file..."
          encResult <- encrypt_haskell pubKey plaintext
          
          case encResult of
            Left err -> putStrLn $ "Encryption error: " ++ err
            Right encryptedB64 -> do
              -- Decode base64 to get raw binary
              case B64.decode encryptedB64 of
                Left decErr -> putStrLn $ "Base64 decode error: " ++ decErr
                Right encryptedRaw -> do
                  -- Save raw encrypted data to file to confirm it's generating partial packets on 3mb
                  BS.writeFile "../rust/seqenc.txt" encryptedRaw
                  putStrLn $ "Encrypted data saved to seqenc.txt (" ++ show (BS.length encryptedRaw) ++ " bytes, raw binary)"
                  putStrLn $ "Base64 version was " ++ show (BS.length encryptedB64) ++ " bytes"
                  
                  -- Test with file content
                  testEncryptDecrypt pubKey privKey plaintext "File content"
                  
                  -- Test with various sizes to verify packet streaming
                  result1 <- testEncryptDecrypt pubKey privKey (BS.take 100 plaintext) "Small packet (100 bytes)"
                  result2 <- testEncryptDecrypt pubKey privKey (BS.take 1024 plaintext) "Medium packet (1 KB)"
                  result3 <- testEncryptDecrypt pubKey privKey (BS.take 8192 plaintext) "Large packet (8 KB)"
                  
                  putStrLn $ "\n=====================================\nResults\n====================================="
                  putStrLn $ "All tests passed: " ++ show (all id [result1, result2, result3])
