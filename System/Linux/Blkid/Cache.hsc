{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Basic routines to work with @libblkid@ cache.
module System.Linux.Blkid.Cache
    ( CacheT
    , withCache
    , gcCache
    , probeAll
    , probeAllRemovable
    , probeAllNew
    , verify
    , evaluateTagUsingCache
    , evaluateSpecUsingCache
    , Device
    , getDevices
    , getDevicesWithTag
    , deviceGetTags
    , deviceHasTag
    , findDeviceWithTag
    , DevFlags(..)
    , getDevice
    , getDevname
    , getTagValue
    , initDebug    ) where

import Control.Applicative (Alternative, Applicative)
import Control.Exception (bracket)
import Control.Monad (MonadPlus)
import Control.Monad.Base
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.State
import Foreign
import Foreign.C
import System.Linux.Blkid.Evaluate (Tag(..))

#include <blkid/blkid.h>

foreign import ccall "stdlib.h free"
  c_free :: Ptr a -> IO ()

type BlkidCache = Ptr BlkidStructCache

data BlkidStructCache

-- | An operation using the @blkid.tab@ cache file, and returning a
-- value of type @a@. The computation is not performed until
-- @'withCache'@ is called.
newtype CacheT m a = CacheT {unCacheT :: StateT BlkidCache m a}
    deriving (Alternative, Applicative, Functor, Monad
             , MonadFix, MonadIO, MonadPlus, MonadTrans)

instance MonadBase b m => MonadBase b (CacheT m) where
    liftBase = liftBaseDefault

instance MonadTransControl CacheT where
    newtype StT CacheT a = StCache {unStCache :: StT (StateT BlkidCache) a}
    liftWith = defaultLiftWith CacheT unCacheT StCache
    restoreT = defaultRestoreT CacheT unStCache

instance MonadBaseControl b m => MonadBaseControl b (CacheT m) where
    newtype StM (CacheT m) a = StMCache {unStMCache :: ComposeSt CacheT m a}
    liftBaseWith = defaultLiftBaseWith StMCache
    restoreM     = defaultRestoreM   unStMCache

-- | Run a @'CacheT'@ operation using the given cache file, or the
-- default (@\/run\/blkid\/blkid.tab@) if @'Nothing'@ is passed. After
-- completing the computation, the new cache is written to the file if
-- the user has sufficent privileges.
withCache :: MonadBaseControl IO m => Maybe FilePath -> CacheT m a -> m a
withCache mpath (CacheT (StateT f)) =
    liftBaseOp (bracket getCache put_cache) (\c -> f c >>= return . fst)
    where getCache =
              maybeWith withCString mpath $ \cstr ->
                  alloca $ \p ->
                      do throwIfNeg_
                           (\e -> "blkid_get_cache returned " ++ show e)
                           (get_cache p cstr)
                         peek p

type BlkidDev = Ptr Device

-- | A device as represented in the @blkid.tab@ cache file.
data Device = Device (ForeignPtr Device) String

withDevice :: Device -> (BlkidDev -> IO a) -> IO a
withDevice (Device p _) = withForeignPtr p

toDevice :: BlkidDev -> IO Device
toDevice p = do fp <- newForeignPtr_ p
                name <- dev_devname p >>= peekCString
                return (Device fp name)

instance Show Device where
    show (Device _ name) = name

-- | Removes garbage (non-existing devices) from the cache.
gcCache :: MonadBase IO m => CacheT m ()
gcCache = CacheT $ StateT $ \c -> do liftBase $ gc_cache c
                                     return ((),c)

-- | Probes all block devices.
probeAll :: MonadBase IO m => CacheT m ()
probeAll =
    CacheT $ StateT $ \c ->
        do liftBase $ throwIfNeg_ (\e -> "blkid_probe_all returned " ++ show e)
                        (probe_all c)
           return ((),c)

-- | The libblkid probing is based on devices from
-- @\/proc\/partitions@ by default. This file usually does not contain
-- removable devices (e.g. CDROMs) and this kind of devices are
-- invisible for libblkid.
--
-- This function adds removable block devices to cache (probing is
-- based on information from the @\/sys@ directory). Don't forget that
-- removable devices (floppies, CDROMs, ...) could be pretty
-- slow. It's very bad idea to call this function by default.
--
-- Note that devices which were detected by this function won't be
-- written to @blkid.tab@ cache file.
probeAllRemovable :: MonadBase IO m => CacheT m ()
probeAllRemovable =
    CacheT $ StateT $ \c ->
        do liftBase $ throwIfNeg_ (\e ->
                                   "blkid_probe_all_removable returned " ++
                                   show e) (probe_all_removable c)
           return ((),c)

-- | Probes all new block devices.
probeAllNew :: MonadBase IO m => CacheT m ()
probeAllNew =
    CacheT $ StateT $ \c ->
        do liftBase $ throwIfNeg_ (\e -> "blkid_probe_all_new returned " ++
                                   show e) (probe_all_new c)
           return ((),c)

-- | Verify that the data in @'Device'@ is consistent with what is on
-- the actual block device (using the devname field only).  Normally
-- this will be called when finding items in the cache, but for long
-- running processes is also desirable to revalidate an item before
-- use.
verify :: MonadBase IO m => Device -> CacheT m Device
verify dev =
    CacheT $ StateT $ \c ->
        do dev' <- liftBase $ withDevice dev $ \d ->
                   blkid_verify c d >>= toDevice
           return (dev',c)

-- | Get the list of tags and values for the given @'Device'@.
deviceGetTags :: MonadBase IO m => Device -> m [(String, String)]
deviceGetTags dev =
    liftBase $ withDevice dev $ \d -> bracket (begin d) tag_iterate_end getTag
    where begin ptr = do it <- tag_iterate_begin ptr
                         if it == nullPtr
                           then fail "blkid_tag_iterate_begin returned NULL"
                           else return it
          getTag iter = alloca $ \t ->
                            alloca $ \v ->
                                do e <- tag_next iter t v
                                   if e < 0
                                     then return []
                                     else do typ <- peek t >>= peekCString
                                             val <- peek v >>= peekCString
                                             xs <- getTag iter
                                             return ((typ,val):xs)

-- | Check if @'Device'@ has the give tag and value.
deviceHasTag :: MonadBase IO m => Device -> String -> String -> m Bool
deviceHasTag dev typ val =
    liftBase $ withDevice dev $ \d ->
        withCString typ $ \t ->
            withCString val $ \v ->
                dev_has_tag d t v >>= return . toBool

-- | Find a @'Device'@ in cache matching the given tag and value.
findDeviceWithTag :: MonadBase IO m => String -> String -> CacheT m (Maybe Device)
findDeviceWithTag typ val =
    CacheT $ StateT $ \c ->
        do mdev <- liftBase $ withCString typ $ \t ->
                   withCString val $ \v ->
                       find_dev_with_tag c t v >>= maybePeek toDevice
           return (mdev,c)

-- | Flags for @'getDevice'@.
data DevFlags = Find
              | Create
              | Verify
              | Normal
                deriving (Eq, Show)

fromDevFlags :: Num a => DevFlags -> a
fromDevFlags Find = #{const BLKID_DEV_FIND}
fromDevFlags Create = #{const BLKID_DEV_CREATE}
fromDevFlags Verify = #{const BLKID_DEV_VERIFY}
fromDevFlags Normal = #{const BLKID_DEV_NORMAL}

-- | Get the device in cache with the given name.
getDevice :: MonadBase IO m => String -> DevFlags -> CacheT m (Maybe Device)
getDevice nam fl =
     CacheT $ StateT $ \c ->
        do mdev <- liftBase $ withCString nam $ \n ->
                   get_dev c n (fromDevFlags fl) >>= maybePeek toDevice
           return (mdev,c)

-- | Get the device name of the device in cache matching the given tag
-- and value.
getDevname :: MonadBase IO m => String -> String -> CacheT m (Maybe String)
getDevname tok val =
    CacheT $ StateT $ \c ->
        do mstr <- liftBase $ withCString tok $ \t ->
                   withCString val $ \v ->
                       bracket (get_devname c t v) c_free (maybePeek peekCString)
           return (mstr,c)

-- | Get the value of a tag given the tag and device name.
getTagValue :: MonadBase IO m => String -> String -> CacheT m (Maybe String)
getTagValue tok nam =
    CacheT $ StateT $ \c ->
        do mstr <- liftBase $ withCString tok $ \t ->
                   withCString nam $ \n ->
                       bracket (get_tag_value c t n) c_free (maybePeek peekCString)
           return (mstr,c)

devIterate :: MonadBase IO m => Maybe String -> String -> CacheT m [Device]
devIterate mtok val =
    CacheT $ StateT $ \c ->
        do devs <- liftBase $ bracket (getIter c) dev_iterate_end getDev
           return (devs,c)
    where getIter p = do it <- dev_iterate_begin p
                         if it == nullPtr
                           then fail "blkid_dev_iterate_begin returned NULL" 
                           else case mtok of
                                  Nothing -> return it
                                  Just tok ->
                                      withCString tok $ \t ->
                                      withCString val $ \v ->
                                      do throwIfNeg_ (\e -> "blkid_dev_set_search returned " ++ show e)
                                           (dev_set_search it t v)
                                         return it
          getDev it = alloca $ \pd -> do e <- dev_next it pd
                                         if e < 0
                                           then return []
                                           else do d <- peek pd >>= toDevice
                                                   ds <- getDev it
                                                   return (d:ds)

-- | Get the list of devices in cache.
getDevices :: MonadBase IO m => CacheT m [Device]
getDevices = devIterate Nothing []

-- | Get the list of devices in cache matching the given tag and value.
getDevicesWithTag :: MonadBase IO m => String -> String -> CacheT m [Device]
getDevicesWithTag tok val = devIterate (Just tok) val

-- | Get the partition or filesystem device with the given @'Tag'@,
-- using the cache file.
evaluateTagUsingCache :: MonadBase IO m => Tag -> CacheT m (Maybe String)
evaluateTagUsingCache (Label str) = evalTag "LABEL" str
evaluateTagUsingCache (PartLabel str) = evalTag "PARTLABEL" str
evaluateTagUsingCache (UUID str) = evalTag "UUID" str
evaluateTagUsingCache (PartUUID str) = evalTag "PARTUUID" str

evalTag :: MonadBase IO m => String -> String -> CacheT m (Maybe String)
evalTag tok val =
    CacheT $ StateT $ \c ->
        liftBase $ alloca $ \p ->
            do poke p c
               mstr <- bracket (withCString tok $ \t ->
                                    withCString val $ \v ->
                                        evaluate_tag t v p)
                       c_free
                       (maybePeek peekCString)
               c' <- peek p
               return (mstr,c')

-- | Get the desired partition or filesystem device, using the cache
-- file.
evaluateSpecUsingCache :: MonadBase IO m => String -> CacheT m (Maybe String)
evaluateSpecUsingCache spec =
    CacheT $ StateT $ \c ->
        liftBase $ alloca $ \p ->
            do poke p c
               mstr <- bracket (withCString spec $ \s -> evaluate_spec s p)
                       c_free
                       (maybePeek peekCString)
               c' <- peek p
               return (mstr,c')

-- | @0xffff@ to enable full debuging.
--
-- If the mask is not specified then this function reads
-- @LIBBLKID_DEBUG@ environment variable to get the mask.
--
-- Already initialized debugging stuff cannot be changed. It does not
-- have effect to call this function twice.
initDebug :: Int -> IO ()
initDebug = init_debug . fromIntegral

type BlkidDevIterate = Ptr DevIterate

data DevIterate

type BlkidTagIterate = Ptr TagIterate

data TagIterate

foreign import ccall "blkid_init_debug" init_debug :: CInt -> IO ()

foreign import ccall "blkid_put_cache" put_cache :: BlkidCache -> IO ()

foreign import ccall "blkid_get_cache" get_cache :: Ptr BlkidCache
                                                 -> CString
                                                 -> IO CInt

foreign import ccall "blkid_gc_cache" gc_cache :: BlkidCache -> IO ()

foreign import ccall "blkid_dev_devname" dev_devname :: BlkidDev -> IO CString

foreign import ccall "blkid_dev_iterate_begin"
  dev_iterate_begin :: BlkidCache -> IO BlkidDevIterate

foreign import ccall "blkid_dev_set_search" dev_set_search :: BlkidDevIterate
                                                           -> CString
                                                           -> CString
                                                           -> IO CInt

foreign import ccall "blkid_dev_next" dev_next :: BlkidDevIterate
                                               -> Ptr BlkidDev
                                               -> IO CInt

foreign import ccall "blkid_dev_iterate_end" dev_iterate_end :: BlkidDevIterate
                                                             -> IO ()

foreign import ccall "blkid_probe_all" probe_all :: BlkidCache -> IO CInt

foreign import ccall "blkid_probe_all_new" probe_all_new :: BlkidCache
                                                         -> IO CInt

foreign import ccall "blkid_probe_all_removable"
  probe_all_removable :: BlkidCache -> IO CInt

foreign import ccall "blkid_get_dev"
  get_dev :: BlkidCache -> CString -> CInt -> IO BlkidDev

foreign import ccall "blkid_verify"
  blkid_verify :: BlkidCache -> BlkidDev -> IO BlkidDev

foreign import ccall "blkid_get_tag_value"
  get_tag_value :: BlkidCache -> CString -> CString -> IO CString

foreign import ccall "blkid_get_devname"
  get_devname :: BlkidCache -> CString -> CString -> IO CString

foreign import ccall "blkid_tag_iterate_begin"
  tag_iterate_begin :: BlkidDev -> IO BlkidTagIterate

foreign import ccall "blkid_tag_next"
  tag_next :: BlkidTagIterate -> Ptr CString -> Ptr CString -> IO CInt

foreign import ccall "blkid_tag_iterate_end"
  tag_iterate_end :: BlkidTagIterate -> IO ()

foreign import ccall "blkid_dev_has_tag"
  dev_has_tag :: BlkidDev -> CString -> CString -> IO CInt

foreign import ccall "blkid_find_dev_with_tag"
  find_dev_with_tag :: BlkidCache -> CString -> CString -> IO BlkidDev

foreign import ccall "blkid_evaluate_tag" evaluate_tag :: CString
                                                       -> CString
                                                       -> Ptr BlkidCache
                                                       -> IO CString

foreign import ccall "blkid_evaluate_spec" evaluate_spec :: CString
                                                         -> Ptr BlkidCache
                                                         -> IO CString
