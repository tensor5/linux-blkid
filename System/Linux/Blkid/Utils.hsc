{-# LANGUAGE ForeignFunctionInterface #-}

-- | Utilities for low-level and high-level API.
module System.Linux.Blkid.Utils
    ( Size
    , devnoToDevname
    , devnoToWholedisk
    , getDevSize
    , libraryVersion
    , libraryReleaseDate
    , sendUevent
    ) where

import Control.Exception (bracket)
import Foreign
import Foreign.C
import System.Posix.Types (CDev(..), Fd(..))

#include <blkid/blkid.h>

foreign import ccall "stdlib.h free"
  c_free :: Ptr a -> IO ()

type BlkidLoff = #{type blkid_loff_t}

-- | Size of a device or partition.
type Size = Word64

-- | Find the pathname to a block device with a given device number.
--
-- >>> devnoToDevname 0x0800
-- Just "/dev/sda"
devnoToDevname :: CDev -> IO (Maybe String)
devnoToDevname d = bracket (devno_to_devname d) c_free (maybePeek peekCString)

-- | Convert the @'CDev'@ device number to the /name/ of the whole
-- disk. The function DOES NOT return full device name. The @'CDev'@
-- argument could be partition or whole disk -- both are converted.
--
-- For example: sda1, 0x0801 --> sda, 0x0800
--
-- >>> devnoToWholedisk 0x0801
-- ("sda",2048)
devnoToWholedisk :: CDev -> IO (String, CDev)
devnoToWholedisk d =
    allocaBytes 32 $ \pc ->
        alloca $ \pd ->
            do throwIfNeg_ (\e -> "blkid_devno_to_wholedisk returned " ++ show e) (devno_to_wholedisk d pc 32 pd)
               str <- peekCString pc
               d1 <- peek pd
               return (str, d1)

-- | Send an event @'String'@ for a device.
sendUevent :: FilePath -> String -> IO ()
sendUevent path str =
    withCString path $ \p ->
        withCString str $ \s ->
            throwIfNeg_ (\e -> "blkid_send_uevent returned " ++ show e) (send_uevent p s)

-- | Get the size of a device given its file descriptor.
getDevSize :: Fd -> IO Size
getDevSize (Fd  n) = get_dev_size n >>= return . fromIntegral


-- | @libblkid@ version.
libraryVersion :: String
libraryVersion = #{const_str BLKID_VERSION}

-- | @libblkid@ release date.
libraryReleaseDate :: String
libraryReleaseDate = #{const_str BLKID_DATE}

foreign import ccall "blkid_devno_to_devname" devno_to_devname :: CDev
                                                               -> IO CString

foreign import ccall "blkid_devno_to_wholedisk" devno_to_wholedisk :: CDev
                                                                   -> CString
                                                                   -> CSize
                                                                   -> Ptr CDev
                                                                   -> IO CInt


foreign import ccall "blkid_get_dev_size" get_dev_size :: CInt -> IO BlkidLoff

foreign import ccall "blkid_send_uevent"
  send_uevent :: CString -> CString -> IO CInt
