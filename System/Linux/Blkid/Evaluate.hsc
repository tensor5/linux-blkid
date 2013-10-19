{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Tags and Spec evaluation.
module System.Linux.Blkid.Evaluate
    ( Tag(..)
    , evaluateTag
--    , evaluateTagUsingCache
    , evaluateSpec
--    , evaluateSpecUsingCache
    ) where

import Control.Exception (bracket)
import Foreign
import Foreign.C

#include <blkid/blkid.h>

foreign import ccall "stdlib.h free"
  c_free :: Ptr a -> IO ()

type BlkidCache = Ptr BlkidStructCache

data BlkidStructCache

-- | Partition or filesystem tag.
data Tag = Label String
         | PartLabel String
         | UUID String
         | PartUUID String
           deriving (Eq, Show)

{-
evalTag :: MonadIO m => String -> String -> BlkidCache -> m (Maybe String)
evalTag tok val cac =
    liftIO $ bracket $ 
               (withCString tok $ \t ->
                    withCString val $ \v ->
                        with cac \c ->
                            evaluate_tag t v c)
               c_free
               (maybePeek peekCString)
-}

-- | Get the partition or filesystem device with the given @'Tag'@.
--
-- Example:
--
-- >>> evaluateTag (PartLabel "EFI System Partition")
-- Just "/dev/sda1"
evaluateTag :: Tag -> IO (Maybe String)
evaluateTag (Label str) = evalTag "LABEL" str
evaluateTag (PartLabel str) = evalTag "PARTLABEL" str
evaluateTag (UUID str) = evalTag "UUID" str
evaluateTag (PartUUID str) = evalTag "PARTUUID" str

evalTag :: String -> String -> IO (Maybe String)
evalTag tok val =
    bracket (withCString tok $ \t ->
                 withCString val $ \v ->
                     evaluate_tag t v nullPtr)
    c_free
    (maybePeek peekCString)
{-
evaluateTagUsingCache :: MonadIO m => String -> String -> CacheT m (Maybe String)
evaluateTagUsingCache tok val =
    CacheT $ StateT $ \c ->
        do mstr <- evalTag tok val c
           return (mstr,c)

evalSpec :: MonadIO m => String -> Ptr BlkidCache -> m (Maybe String)
evalSpec spec c =
    liftIO $ bracket
               (withCString spec $ \s -> evaluate_spec s c)
               c_free
               (maybePeek peekCString)
-}

-- | Get the desired partition or filesystem device.
--
-- Examples:
--
-- >>> evaluateSpec "LABEL=EFI"
-- Just "/dev/sda1"
--
-- >>> evaluateSpec "/dev/disk/by-label/EFI"
-- Just "/dev/sda1"
evaluateSpec :: String -> IO (Maybe String)
evaluateSpec spec =
    bracket
    (withCString spec $ \s -> evaluate_spec s nullPtr)
    c_free
    (maybePeek peekCString)

{-
evaluateSpecUsingCache :: MonadIO m => String -> CacheT m (Maybe String)
evaluateSpecUsingCache spec =
    CacheT $ StateT $ \c ->
        do mstr <- evalSpec spec c
           return (mstr,c)
-}
foreign import ccall "blkid_evaluate_tag" evaluate_tag :: CString
                                                       -> CString
                                                       -> Ptr BlkidCache
                                                       -> IO CString

foreign import ccall "blkid_evaluate_spec" evaluate_spec :: CString
                                                         -> Ptr BlkidCache
                                                         -> IO CString
