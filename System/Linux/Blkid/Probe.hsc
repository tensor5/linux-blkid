{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level probing functions.
module System.Linux.Blkid.Probe
    ( -- * Low-level probing
      Probe
--    , ProbeT
--    , useProbe
    , newProbe
    , newProbeFromFilename
    , probeGetDevno
    , probeGetFd
    , probeGetSectorsize
    , probeGetSectors
    , probeGetSize
    , probeGetOffset
    , probeGetWholediskDevno
    , probeSetDevice
    , probeIsWholedisk
    , probeStepBack
    , resetProbe
    -- * Low-level tags
    , doProbe
    , doFullprobe
    , doSafeprobe
    , doWipe
    , probeGetValues
    , probeHasValue
    , probeLookupValue
    -- * Miscellaneous utils
    , Offset
    , Size
    -- * Topology information
    , Topology (..)
    , probeEnableTopology
    , probeGetTopology
    -- * Superblocks probing
    , probeEnableSuperblocks
    , knownFstype
    , superblocksGetName
    , probeFilterSuperblocksTypeNotIn
    , probeFilterSuperblocksTypeOnlyIn
    , Usage (..)
    , probeFilterSuperblocksUsageNotIn
    , probeFilterSuperblocksUsageOnlyIn
    , probeInvertSuperblocksFilter
    , probeResetSuperblocksFilter
    , Superblock(..)
    , defaultSuperblock
    , probeSetSuperblocksFlags
    -- * Partitions probing
    , Partition(..)
--    , Partlist
    , PartTable(..)
    , probeEnablePartitions
    , PartsFlags(..)
    , probeSetPartitionsFlags
    , probeFilterPartitionsTypeNotIn
    , probeFilterPartitionsTypeOnlyIn
    , probeInvertPartitionsFilter
    , probeResetPartitionsFilter
    , knownPttype
{-
    , partitionGetName
    , partitionGetFlags
    , partitionGetPartno
    , partitionGetSize
    , partitionGetStart
    , partitionGetTable
    , partitionGetType
    , partitionGetTypeString
    , partitionGetUUID
    , partitionIsExtended
    , partitionIsLogical
    , partitionIsPrimary
-}
    , probeDevnoToPartition
{-
    , parttableGetOffset
    , parttableGetParent
    , parttableGetType
-}
--    , probeGetPartitions
    , probeGetPartList
    ) where

import Control.Exception (bracket)
--import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
--import Control.Monad.Trans.State
import Foreign
import Foreign.C
import System.Linux.Blkid.Utils (Size)
import System.Posix.Types (CDev(..), Fd(..))

#include <blkid/blkid.h>
{-
newtype ProbeT m a = ProbeT { runProbeT :: StateT BlkidProbe m a}

instance MonadTrans ProbeT where
    lift = ProbeT . lift

instance Functor f => Functor (ProbeT f) where
    fmap f = ProbeT . (fmap f) . runProbeT

instance Monad m => Monad (ProbeT m) where
    return = ProbeT . return
    (ProbeT m) >>= f = ProbeT (m >>= runProbeT . f)
    fail = ProbeT . fail

instance MonadIO m => MonadIO (ProbeT m) where
    liftIO = ProbeT . liftIO

useProbe :: MonadIO m => FilePath -> ProbeT m a -> m a
useProbe path (ProbeT f) =
    do pr <- liftIO $ withCString path $ \p ->
             throwIfNull "blkid_new_probe_from_filename returned NULL" $
                         new_probe_from_filename p
       (a,pr') <- runStateT f pr
       liftIO $ free_probe pr'
       return a
-}
foreign import ccall "stdlib.h free"
  c_free :: Ptr a -> IO ()

type BlkidProbe = Ptr Probe

-- | Low-level probing setting.
newtype Probe = Probe (ForeignPtr Probe)

withProbe :: Probe -> (Ptr Probe -> IO a) -> IO a
withProbe (Probe p) = withForeignPtr p

toProbe :: Ptr Probe -> IO Probe
toProbe p = newForeignPtr p_free_probe p >>= return . Probe

type BlkidTopology = Ptr Topology

-- | Offset of a device or partition.
type Offset = Word64

-- | Data structure for topology information.
data Topology = Topology { alignmentOffset :: Offset
                         , minimumIOSize :: Size
                         , optimalIOSize :: Size
                         , logicalSectorSize :: Size
                         , physicalSectorSize :: Size
                         } deriving (Eq, Show)

type BlkidPartlist = Ptr Partlist

data Partlist

type BlkidParttable = Ptr PartTable

type BlkidPartition = Ptr Partition

-- | The information about a partition as returned by
-- @'probeDevnoToPartition'@ and @'probeGetPartList'@.
data Partition = Partition
    { partitionStart     :: Offset
    , partitionSize      :: Size
    , partitionType      :: Int -- ^ Partition type.
    , partitionTypeStr   :: Maybe String -- ^ Partition type string or
                                         -- @'Nothing'@. The type
                                         -- string is supported by a
                                         -- small subset of partition
                                         -- tables (e.g Mac and EFI
                                         -- GPT). Note that GPT uses
                                         -- type UUID and this
                                         -- function returns this UUID
                                         -- as string.
    , partitionFlags     :: Word64 -- ^ Partition flags (or attributes
                                   -- for @gpt@).
    , partitionNumber    :: Int -- ^ Proposed partition number
                                -- (e.g. 'N' from sda'N') or
                                -- @'Control.Exception.throw'@ an
                                -- error. Note that the number is
                                -- generated by library independenly
                                -- on your OS.
    , partitionUUID      :: Maybe String -- ^ Partition UUID string if
                                         -- supported by partition
                                         -- table (e.g. GPT) or
                                         -- @'Nothing'@.
    , partitionName      :: Maybe String -- ^ Partition name if
                                         -- supported by partition
                                         -- table (e.g. Mac) or
                                         -- @'Nothing'@.
    , partitionPartTable :: PartTable
    , isExtended :: Bool -- ^ Check if partition is extended (dos,
                         -- windows or linux) partition.
    , isLogical :: Bool -- ^ Check if partition is logical partition.
                        --
                        -- Note: that this function returns @'True'@
                        -- for all partitions in all nested partition
                        -- tables (e.g. BSD labels).
    , isPrimary :: Bool -- ^ Check if partition is primary partition.
                        --
                        -- Note: this function returns @'False'@ for
                        -- DOS extended partitions and all partitions
                        -- in nested partition tables.
    } deriving (Eq, Show)

toPartition :: BlkidPartition -> IO Partition
toPartition p =
    do sta <- partition_get_start p >>= return . fromIntegral
       siz <- partition_get_size p >>= return . fromIntegral
       typ <- partition_get_type p >>= return . fromIntegral
       str <- partition_get_type_string p >>= maybePeek peekCString
       fla <- partition_get_flags p >>= return . fromIntegral
       pno <- partition_get_partno p >>= return . fromIntegral
       uuid <- partition_get_uuid p >>= maybePeek peekCString
       nam <- partition_get_name p >>= maybePeek peekCString
       tab <- partition_get_table p >>= toPartTable
       ext <- partition_is_extended p >>= return . toBool
       logi <- partition_is_logical p >>= return . toBool
       pri <- partition_is_primary p >>= return . toBool
       return $ Partition sta siz typ str fla pno uuid nam tab ext logi pri

-- | The information about a partition table, as returned by
-- @'probeGetPartList'@.
data PartTable = PartTable
    { parttableType   :: String -- ^ Partition table type (type name,
                                -- e.g. \"dos\", \"gpt\", ...).
    , parttableOffset :: Offset -- ^ Position (in bytes) of the
                                -- partition table.
                                --
                                -- Note: the position is relative to
                                -- begin of the device as defined by
                                -- @'probeSetDevice'@ for primary
                                -- partition table, and relative to
                                -- parental partition for nested
                                -- patition tables.
    , parttableParent :: Maybe Partition -- ^ Parent for nested
                                         -- partitition tables or
                                         -- @'Nothing'@.
    , parttableID :: Maybe String -- ^ GPT disk UUID or DOS disk ID
                                  -- (in hex format).
    } deriving (Eq, Show)

toPartTable :: BlkidParttable -> IO PartTable
toPartTable pt =
 do typ <- parttable_get_type pt >>= peekCString
    off <- parttable_get_offset pt >>= return . fromIntegral
    par <- parttable_get_parent pt >>= maybePeek toPartition
    mid <- parttable_get_id pt >>= maybePeek peekCString
    return $ PartTable typ off par mid

-- | Usage filter to use with @'probeFilterSuperblocksUsageNotIn'@ and
-- @'probeFilterSuperblocksUsageOnlyIn'@.
data Usage = Filesystem
           | Raid
           | Crypto
           | Other
             deriving (Eq, Show)

fromUsage :: Num a => Usage -> a
fromUsage Filesystem = #{const BLKID_USAGE_FILESYSTEM}
fromUsage Raid = #{const BLKID_USAGE_RAID}
fromUsage Crypto = #{const BLKID_USAGE_CRYPTO}
fromUsage Other = #{const BLKID_USAGE_OTHER}

toUsage :: (Eq a, Num a) => a -> Maybe Usage
toUsage #{const BLKID_USAGE_FILESYSTEM} = Just Filesystem
toUsage #{const BLKID_USAGE_RAID} = Just Raid
toUsage #{const BLKID_USAGE_CRYPTO} = Just Crypto
toUsage #{const BLKID_USAGE_OTHER} = Just Other
toUsage _ = Nothing


-- | Create a new @'Probe'@ data structure, to use with
-- @'probeSetDevice'@.
newProbe :: IO Probe
newProbe = do p <- new_probe
              if p == nullPtr
                then fail "blkid_new_probe returned NULL"
                else toProbe p

-- | This function is same as
--
-- > \path -> do fd <- openFd path ReadOnly Nothing defaultFileFlags
-- >             pr <- newProbe
-- >             probeSetDevice pr fd 0 0
-- >             return pr
newProbeFromFilename :: FilePath -> IO Probe
newProbeFromFilename path =
    withCString path $ \cstr ->
        do p <- new_probe_from_filename cstr
           if p == nullPtr
             then fail "blkid_new_probe_from_filename returned NULL"
             else toProbe p 

-- | Get the device number of the probed device.
probeGetDevno :: Probe -> IO (Maybe CDev)
probeGetDevno pr = withProbe pr $ \p -> do d <- probe_get_devno p
                                           case d of
                                             0 -> return Nothing
                                             _ -> return $ Just d

-- | Get the file descriptor of the probed device.
probeGetFd :: Probe -> IO Fd
probeGetFd pr = withProbe pr $ \p -> probe_get_fd p >>= return . Fd

-- | Get the logical sector size of the probed device.
probeGetSectorsize :: Maybe Probe -> IO Word
probeGetSectorsize mpr =
    maybeWith withProbe mpr $ \p -> probe_get_sectorsize p >>= return . fromIntegral

-- | Get the 512-byte sector count of the probed device.
probeGetSectors :: Probe -> IO Word64
probeGetSectors pr =
    withProbe pr $ \p ->
        throwIfNeg (\e -> "blkid_probe_get_sectors returned " ++ show e) (probe_get_sectors p) >>= return . fromIntegral

-- | Return the size in bytes of the probing area as defined by
-- @'probeSetDevice'@. If the size of the probing area is unrestricted
-- then this function returns the real size of device. See also
-- @'System.Linux.Blkid.Utils.getDevSize'@.
probeGetSize :: Probe -> IO Size
probeGetSize pr =
    withProbe pr $ \p ->
        throwIfNeg (\e -> "blkid_probe_get_size returned " ++ show e) (probe_get_size p) >>= return . fromIntegral

-- | Return the offset in bytes of the probing area as defined by
-- @'probeSetDevice'@.
probeGetOffset :: Probe -> IO Offset
probeGetOffset pr =
    withProbe pr $ \p ->
        throwIfNeg (\e -> "blkid_probe_get_offset returned " ++ show e) (probe_get_offset p) >>= return . fromIntegral

-- | Return device number of the wholedisk, or 0 for regular files.
probeGetWholediskDevno :: Probe -> IO (Maybe CDev)
probeGetWholediskDevno pr =
    withProbe pr $ \p -> do d <- probe_get_wholedisk_devno p
                            case d of
                              0 -> return Nothing
                              _ -> return $ Just d

-- | Assigns the device to @'Probe'@ control struct, resets internal
-- buffers and resets the current probing.
probeSetDevice :: Probe
               -> Fd -- ^ Device file descriptor
               -> Offset -- ^ Begin of probing area
               -> Size -- ^ Size of probing area (zero means whole
                       -- device/file)
               -> IO ()
probeSetDevice pr (Fd n) off size =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_set_device returned " ++ show e)
        (probe_set_device p n (fromIntegral off) (fromIntegral size))

-- | Check if device is whole-disk.
probeIsWholedisk :: Probe -> IO Bool
probeIsWholedisk pr =
    withProbe pr $ \p -> probe_is_wholedisk p >>= return . toEnum . fromIntegral

-- | Move pointer to the probing chain one step back -- it means that
-- the previously used probing function will be called again in the
-- next @'doProbe'@ call.
--
-- This is necessary for example if you erase or modify on-disk
-- superblock according to the current libblkid probing result.
probeStepBack :: Probe -> IO ()
probeStepBack pr =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_step_back returned " ++ show e) $
        probe_step_back p

-- | Zeroize probing results and resets the current probing (this has
-- impact to @'doProbe'@ only). This function does not touch probing
-- filters and keeps assigned device.
resetProbe :: Probe -> IO ()
resetProbe pr = withProbe pr reset_probe

-- | Enable/disable the topology probing for non-binary interface.
probeEnableTopology :: Probe -> Bool -> IO ()
probeEnableTopology pr b =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_enable_topology returned " ++ show e)
                       (probe_enable_topology p (fromBool b))

-- | Get topology information. 
--
-- This function is independent on @'doProbe'@, @'doSafeprobe'@,
-- @'doFullprobe'@ and @'probeEnableTopology'@ functions.
probeGetTopology :: Probe -> IO Topology
probeGetTopology pr =
    withProbe pr $ \p -> bracket
                         (probe_get_topology p)
                         c_free
                         (\t ->
                          if t == nullPtr
                              then fail "blkid_probe_get_topology returned NULL"
                              else do ao <- topology_get_alignment_offset t
                                      mio <- topology_get_minimum_io_size t
                                      oio <- topology_get_optimal_io_size t
                                      ls <- topology_get_logical_sector_size t
                                      ps <- topology_get_physical_sector_size t
                                      return (Topology
                                              (fromIntegral ao)
                                              (fromIntegral mio)
                                              (fromIntegral oio)
                                              (fromIntegral ls)
                                              (fromIntegral ps)
                                             ))

-- | Enable/disable the superblocks probing for non-binary interface.
probeEnableSuperblocks :: Probe -> Bool -> IO ()
probeEnableSuperblocks pr b =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_enable_superblocks returned " ++ show e)
                         (probe_enable_superblocks p (fromBool b))

-- | Check if the filesystem is of known type.
knownFstype :: String -> IO Bool
knownFstype str =
    withCString str $ \cstr -> known_fstype cstr >>= return . toBool

-- | Table of superblocks name and usage.
superblocksGetName :: CSize -> IO (Maybe (String, Usage))
superblocksGetName s = alloca $ \p ->
                       alloca $ \q -> do
                         e <- superblocks_get_name s p q
                         case e of
                           0 -> do str <- peek p >>= peekCString
                                   i <- peek q
                                   let Just u = toUsage i
                                   return $ Just (str, u)
                           _ -> return Nothing

withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray xs f = bracket
                        (mapM newCString xs)
                        (mapM_ free)
                        (\ps -> withArray0 nullPtr ps f)

probeFilterSuperblocksType :: CInt -> [String] -> Probe -> IO ()
probeFilterSuperblocksType fl xs pr =
    withProbe pr $ \p ->
        withCStringArray xs $ \cs ->
            throwIfNeg_ (\e -> "blkid_probe_filter_superblocks_type returned " ++ show e)
            (probe_filter_superblocks_type p fl cs)

-- | Probe for all superblocks whose type is not in the list.
probeFilterSuperblocksTypeNotIn :: [String] -> Probe -> IO ()
probeFilterSuperblocksTypeNotIn =
    probeFilterSuperblocksType #{const BLKID_FLTR_NOTIN}

-- | Probe only for superblocks whose type is in the list.
probeFilterSuperblocksTypeOnlyIn :: [String] -> Probe -> IO ()
probeFilterSuperblocksTypeOnlyIn =
    probeFilterSuperblocksType #{const BLKID_FLTR_ONLYIN}

probeFilterSuperblocksUsage :: CInt -> [Usage] -> Probe -> IO ()
probeFilterSuperblocksUsage fl us pr =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_filter_superblocks_usage returned " ++ show e)
                         (probe_filter_superblocks_usage p fl (foldl1 (.|.) $ map fromUsage us))

-- | Probe for all superblocks whose @'Usage'@ is not in the list.
probeFilterSuperblocksUsageNotIn :: [Usage] -> Probe -> IO ()
probeFilterSuperblocksUsageNotIn =
    probeFilterSuperblocksUsage #{const BLKID_FLTR_NOTIN}

-- | Probe only for superblocks whose @'Usage'@ is in the list.
probeFilterSuperblocksUsageOnlyIn :: [Usage] -> Probe -> IO ()
probeFilterSuperblocksUsageOnlyIn =
    probeFilterSuperblocksUsage #{const BLKID_FLTR_ONLYIN}

-- | Invert superblocks probing filter.
probeInvertSuperblocksFilter :: Probe -> IO ()
probeInvertSuperblocksFilter pr =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_invert_superblocks_filter returned " ++ show e) (probe_invert_superblocks_filter p)

-- | Reset superblocks probing filter.
probeResetSuperblocksFilter :: Probe -> IO ()
probeResetSuperblocksFilter pr =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_reset_superblocks_filter returned " ++ show e) (probe_reset_superblocks_filter p)

-- | Superblock probing flags.
data Superblock = Superblock { readLabel    :: Bool
                             , readLabelRaw :: Bool
                             , readUUID     :: Bool
                             , readUUIDRaw  :: Bool
                             , readType     :: Bool
                             , readSecType  :: Bool
                             , readUsage    :: Bool
                             , readVersion  :: Bool
                             , readMagic    :: Bool
                             } deriving (Eq, Show)

-- | Default superblock flags.
defaultSuperblock :: Superblock
defaultSuperblock = Superblock True False True False True True False False False

fromSuperblock :: (Bits a, Num a) => Superblock -> a
fromSuperblock (Superblock l lr u ur t st us v m) =
    (if l then #{const BLKID_SUBLKS_LABEL} else 0) .|.
    (if lr then #{const BLKID_SUBLKS_LABELRAW} else 0) .|.
    (if u then #{const BLKID_SUBLKS_UUID} else 0) .|.
    (if ur then #{const BLKID_SUBLKS_UUIDRAW} else 0) .|.
    (if t then #{const BLKID_SUBLKS_TYPE} else 0) .|.
    (if st then #{const BLKID_SUBLKS_SECTYPE} else 0) .|.
    (if us then #{const BLKID_SUBLKS_USAGE} else 0) .|.
    (if v then #{const BLKID_SUBLKS_VERSION} else 0) .|.
    (if m then #{const BLKID_SUBLKS_MAGIC} else 0)

-- | Set probing flags to the superblocks prober. This function is
-- optional, the default is @'defaultSuperblock'@.
probeSetSuperblocksFlags :: Probe -> Superblock -> IO ()
probeSetSuperblocksFlags pr s =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_set_superblocks_flags returned " ++ show e)
        (probe_set_superblocks_flags p (fromSuperblock s))

-- | Enable/disable the partitions probing for non-binary interface.
probeEnablePartitions :: Probe -> Bool -> IO ()
probeEnablePartitions pr b =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_enable_partitions returned " ++ show e)
                       (probe_enable_partitions p (fromBool b))

-- | Flags for @'probeSetPartitionsFlags'@.
data PartsFlags = PartsFlags { forceGPT     :: Bool
                             , entryDetails :: Bool
                             , magic        :: Bool
                             } deriving (Eq, Show)

fromPartsFlags :: (Bits a, Num a) => PartsFlags -> a
fromPartsFlags (PartsFlags g d m) =
    (if g then #{const BLKID_PARTS_FORCE_GPT} else 0) .|.
    (if d then #{const BLKID_PARTS_ENTRY_DETAILS} else 0) .|.
    (if m then #{const BLKID_PARTS_MAGIC} else 0)

-- | Set probing flags to the partitions prober. This function is optional.
probeSetPartitionsFlags :: Probe -> PartsFlags -> IO ()
probeSetPartitionsFlags pr fl =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_set_partitions_flags returned " ++ show e)
        (probe_set_partitions_flags p (fromPartsFlags fl))

probeFilterPartitionsType :: CInt -> [String] -> Probe -> IO ()
probeFilterPartitionsType i xs pr =
    withProbe pr $ \p ->
        withCStringArray xs $ \arr ->
            throwIfNeg_ (\e -> "blkid_probe_filter_partitions_type returned " ++ show e)
            (probe_filter_partitions_type p i arr)

-- | Probe for all partitions whose type is not in the list.
probeFilterPartitionsTypeNotIn :: [String] -> Probe -> IO ()
probeFilterPartitionsTypeNotIn =
    probeFilterPartitionsType #{const BLKID_FLTR_NOTIN}

-- | Probe only for partitions whose type is the list.
probeFilterPartitionsTypeOnlyIn :: [String] -> Probe -> IO ()
probeFilterPartitionsTypeOnlyIn =
    probeFilterPartitionsType #{const BLKID_FLTR_ONLYIN}

-- | Invert partitions probing filter.
probeInvertPartitionsFilter :: Probe -> IO ()
probeInvertPartitionsFilter pr =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_invert_partitions_filter returned " ++ show e) (probe_invert_partitions_filter p)

-- | Reset partitions probing filter.
probeResetPartitionsFilter :: Probe -> IO ()
probeResetPartitionsFilter pr =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_probe_reset_partitions_filter returned " ++ show e) (probe_reset_partitions_filter p)

-- | Check if the partition if of known type.
knownPttype :: String -> IO Bool
knownPttype str =
    withCString str $ \cstr -> known_pttype cstr >>= return . toBool
{-
-- | Get partition name if supported by partition table (e.g. Mac) or
-- @'Nothing'@.
partitionGetName :: Partition -> IO (Maybe String)
partitionGetName part =
    withPartition part $ \p -> partition_get_name p >>= maybePeek peekCString

-- | get partition flags (or attributes for @gpt@).
partitionGetFlags :: Partition -> IO Word64
partitionGetFlags part =
    withPartition part $ \p -> partition_get_flags p >>= return . fromIntegral

-- | Proposed partitin number (e.g. 'N' from sda'N') or
-- @'Control.Exception.throw'@ an error. Note that the number is
-- generated by library independenly on your OS.
partitionGetPartno :: Partition -> IO Int
partitionGetPartno part =
    withPartition part $ \p ->
        throwIfNeg (\e -> "blkid_partition_get_partno returned " ++ show e) (partition_get_partno p)
                           >>= return . fromIntegral

partitionGetSize :: Partition -> IO Size
partitionGetSize part =
    withPartition part $ \p -> partition_get_size p >>= return . fromIntegral

partitionGetStart :: Partition -> IO Offset
partitionGetStart part =
    withPartition part $ \p -> partition_get_start p >>= return . fromIntegral

partitionGetTable :: Partition -> IO PartTable
partitionGetTable part = withPartition part $ \p ->
                         do q <- partition_get_table p
                            if q == nullPtr
                              then fail
                                       "blkid_partition_get_table returned NULL"
                              else toPartTable q

-- | Get partition type.
partitionGetType :: Partition -> IO Int
partitionGetType part =
    withPartition part $ \p -> partition_get_type p >>= return . fromIntegral

-- | Get partition type string or @'Nothing'@. The type string is
-- supported by a small subset of partition tables (e.g Mac and EFI
-- GPT). Note that GPT uses type UUID and this function returns this
-- UUID as string.
partitionGetTypeString :: Partition -> IO (Maybe String)
partitionGetTypeString part =
    withPartition part $ \p ->
        partition_get_type_string p >>= maybePeek peekCString

-- | Get partition UUID string if supported by partition table
-- (e.g. GPT) or @'Nothing'@.
partitionGetUUID :: Partition -> IO (Maybe String)
partitionGetUUID part =
    withPartition part $ \p -> partition_get_uuid p >>= maybePeek peekCString

-- | Check if partition is extended (dos, windows or linux) partition.
partitionIsExtended :: Partition -> IO Bool
partitionIsExtended part =
    withPartition part $ \p -> partition_is_extended p >>= return . toBool

-- | Check if partition is logical partition.
--
-- Note: that this function returns @'True'@ for all partitions in all
-- nested partition tables (e.g. BSD labels).
partitionIsLogical :: Partition -> IO Bool
partitionIsLogical part =
    withPartition part $ \p -> partition_is_logical p >>= return . toBool

-- | Check if partition is primary partition.
--
-- Note: this function returns @'False'@ for DOS extended partitions
-- and all partitions in nested partition tables.
partitionIsPrimary :: Partition -> IO Bool
partitionIsPrimary part =
    withPartition part $ \p -> partition_is_primary p >>= return . toBool
-}
-- | This function tries to get start and size for devno from @sysfs@
-- and returns a partition which matches with the values from @sysfs@.
--
-- This function is independent on @'doProbe'@, @'doSafeprobe'@,
-- @'doFullprobe'@ and @'probeEnablePartitions'@ functions.
probeDevnoToPartition :: Probe -> CDev -> IO (Maybe Partition)
probeDevnoToPartition pr d =
    withProbe pr $ \p ->
        do pl <- throwIfNull "blkid_probe_get_partitions returned NULL" $
                 probe_get_partitions p
           partlist_devno_to_partition pl d >>= maybePeek toPartition
{-
-- | Get the position (in bytes) of the partition table.
--
-- Note: the position is relative to begin of the device as defined by
-- @'probeSetDevice'@ for primary partition table, and relative to
-- parental partition for nested patition tables.
parttableGetOffset :: PartTable -> IO Offset
parttableGetOffset pt =
    withPartTable pt $ \p -> parttable_get_offset p >>= return . fromIntegral

-- | Get parent for nexted partitition tables or @'Nothing'@.
parttableGetParent :: PartTable -> IO (Maybe Partition)
parttableGetParent pt =
    withPartTable pt $ \p -> parttable_get_parent p >>= maybePeek toPartition

-- | Get partition table type (type name, e.g. \"dos\", \"gpt\", ...).
parttableGetType :: PartTable -> IO String
parttableGetType pt =
    withPartTable pt $ \p -> parttable_get_type p >>= peekCString
-}
-- | Get partition list.
--
-- This function is independent on @'doProbe'@, @'doSafeprobe'@,
-- @'doFullprobe'@ and @'probeEnablePartitions'@ functions.
probeGetPartList :: Probe -> IO (Maybe PartTable, [Partition])
probeGetPartList pr =
    withProbe pr $ \p ->
        do pl <- throwIfNull "blkid_probe_get_partitions returned NULL" $
                 probe_get_partitions p
           mpt <- partlist_get_table pl >>= maybePeek toPartTable
           n <- partlist_numof_partitions pl
           parts <- mapM (getPart pl) [0..n-1]
           return (mpt, parts)
    where getPart plist i =
              throwIfNull "blkid_partlist_get_partition returned NULL"
                              (partlist_get_partition plist i) >>= toPartition
            
-- | Get the list of probed @(key,value)@ pairs.
probeGetValues :: Probe -> IO [(String,String)]
probeGetValues pr =
    withProbe pr $ \p ->
        do n <- throwIfNeg (\e -> "blkid_probe_numof_values returned " ++
                            show e) $ probe_numof_values p
           if n == 0
             then return []
             else mapM (getValue p) [0..n-1]
    where getValue r i =
              alloca $ \pn ->
                  alloca $ \pd ->
                      alloca $ \ps ->
                          do throwIfNeg_ (\e -> "blkid_probe_get_value returned " ++ show e)
                               (probe_get_value r i pn pd ps) 
                             n <- peek pn >>= peekCString
                             d <- peek pd >>= peekCString
                             return (n,d)

-- | Test if a given key has a value.
probeHasValue :: Probe -> String -> IO Bool
probeHasValue pr str =
    withProbe pr $ \p ->
        withCString str $ \cstr ->
            probe_has_value p cstr >>= return . toBool

-- | Lookup the value for the given key, and return it if present.
probeLookupValue :: Probe -> String -> IO (Maybe String)
probeLookupValue pr str =
    withProbe pr $ \p ->
        withCString str $ \cstr ->
            alloca $ \pd ->
                alloca $ \ps ->
                    do e <- probe_lookup_value p cstr pd ps
                       case e of
                         0 -> peek pd >>= peekCString >>= return . Just
                         (-1) -> return Nothing
                         _ -> fail
                              ("blkid_probe_lookup_value returned " ++ show e)

-- | This function erases the current signature detected by
-- @'Probe'@. The @'Probe'@ has to be open in
-- @'System.Posix.IO.ReadWrite'@ mode, and either use
-- @'probeSetSuperblocksFlags'@ with @'readMagic'@ set to @'True'@, or
-- use @'probeSetPartitionsFlags'@ with @'magic'@ set to @'True'@.
--
-- After successful signature removing the @'Probe'@ will be moved one
-- step back and the next @'doProbe'@ call will again call previously
-- called probing function.
doWipe :: Probe -> Bool -> IO ()
doWipe pr b =
    withProbe pr $ \p ->
        throwIfNeg_ (\e -> "blkid_do_wipe returned " ++ show e) (do_wipe p (fromBool b))

-- | Call probing functions in all enabled chains. The superblocks
-- chain is enabled by default. @'doProbe'@ stores result from only
-- one probing function. Use @'probeGetValues'@ to get results from
-- all probing functions in all chains. The probing is reset by
-- @'resetProbe'@ or by filter functions.
--
-- This is string-based @(key,value)@ interface only.
doProbe :: Probe -> IO Bool
doProbe pr =
    withProbe pr $ \p ->
        throwIfNeg (\e -> "blkid_do_probe returned " ++ show e) (do_probe p)
                       >>= return . not . toBool

-- | This function gathers probing results from all enabled
-- chains. Same as @'doSafeprobe'@ but does not check for collision
-- between probing result.
--
-- This is string-based @(key,value)@ interface only.
doFullprobe :: Probe -> IO Bool
doFullprobe pr =
    withProbe pr $ \p ->
        throwIfNeg (\e -> "blkid_do_fullprobe returned " ++ show e)
                       (do_fullprobe p) >>= return . not . toBool

-- | This function gathers probing results from all enabled chains and checks for ambivalent results (e.g. more filesystems on the device).
--
-- This is string-based @(key,value)@ interface only.
--
-- Note about suberblocks chain: the function does not check for
-- filesystems when a RAID signature is detected. The function also
-- does not check for collision between RAIDs. The first detected RAID
-- is returned. The function checks for collision between partition
-- table and RAID signature -- it's recommended to enable partitions
-- chain together with superblocks chain.
doSafeprobe :: Probe
            -> IO (Maybe Bool) -- ^ @'Just' 'True'@ on success,
                               -- @'Just' 'False'@ if nothing is
                               -- detected, @'Nothing'@ if ambivalent
                               -- result is detected.
doSafeprobe pr =
    withProbe pr $ \p ->
        do n <- do_safeprobe p
           case n of
             1 -> return $ Just False
             0 -> return $ Just True
             (-2) -> return Nothing
             e -> fail ("blkid_do_safeprobe returned " ++ show e)

type BlkidLoff = #{type blkid_loff_t}

{-
foreign import ccall "blkid_parse_tag_string"
  parse_tag_string :: CString -> Ptr CString -> Ptr CString -> IO CInt

foreign import ccall "blkid_parse_version_string"
  parse_version_string :: CString -> IO CInt

foreign import ccall "blkid_get_library_version"
  get_library_version :: Ptr CString -> Ptr CString -> IO CInt

foreign import ccall "blkid_encode_string" encode_string :: CString
                                                         -> CString
                                                         -> CSize
                                                         -> IO CInt

foreign import ccall "blkid_safe_string" safe_string :: CString
                                                     -> CString
                                                     -> CSize
                                                     -> IO CInt
-}
foreign import ccall "blkid_new_probe" new_probe :: IO BlkidProbe

foreign import ccall "blkid_new_probe_from_filename"
  new_probe_from_filename :: CString -> IO BlkidProbe

foreign import ccall "&blkid_free_probe"
  p_free_probe :: FunPtr (BlkidProbe -> IO ())

--foreign import ccall "blkid_free_probe" free_probe :: BlkidProbe -> IO ()

foreign import ccall "blkid_reset_probe" reset_probe :: BlkidProbe
                                                     -> IO ()

foreign import ccall "blkid_probe_set_device" probe_set_device :: BlkidProbe
                                                               -> CInt
                                                               -> BlkidLoff
                                                               -> BlkidLoff
                                                               -> IO CInt

foreign import ccall "blkid_probe_get_devno" probe_get_devno :: BlkidProbe
                                                             -> IO CDev

foreign import ccall "blkid_probe_get_wholedisk_devno"
  probe_get_wholedisk_devno :: BlkidProbe -> IO CDev

foreign import ccall "blkid_probe_is_wholedisk" probe_is_wholedisk :: BlkidProbe
                                                                   -> IO CInt

foreign import ccall "blkid_probe_get_size" probe_get_size :: BlkidProbe
                                                           -> IO BlkidLoff

foreign import ccall "blkid_probe_get_offset" probe_get_offset :: BlkidProbe
                                                               -> IO BlkidLoff

foreign import ccall "blkid_probe_get_sectorsize"
  probe_get_sectorsize :: BlkidProbe -> IO CUInt

foreign import ccall "blkid_probe_get_sectors" probe_get_sectors :: BlkidProbe
                                                                 -> IO BlkidLoff

foreign import ccall "blkid_probe_get_fd" probe_get_fd :: BlkidProbe
                                                       -> IO CInt

foreign import ccall "blkid_known_fstype" known_fstype :: CString -> IO CInt

foreign import ccall "blkid_superblocks_get_name"
  superblocks_get_name :: CSize -> Ptr CString -> Ptr CInt -> IO CInt

foreign import ccall "blkid_probe_enable_superblocks"
  probe_enable_superblocks :: BlkidProbe -> CInt -> IO CInt

foreign import ccall "blkid_probe_set_superblocks_flags"
  probe_set_superblocks_flags :: BlkidProbe -> CInt -> IO CInt

foreign import ccall "blkid_probe_reset_superblocks_filter"
  probe_reset_superblocks_filter :: BlkidProbe -> IO CInt

foreign import ccall "blkid_probe_invert_superblocks_filter"
  probe_invert_superblocks_filter :: BlkidProbe -> IO CInt

foreign import ccall "blkid_probe_filter_superblocks_type"
  probe_filter_superblocks_type :: BlkidProbe -> CInt -> Ptr CString -> IO CInt

foreign import ccall "blkid_probe_filter_superblocks_usage"
  probe_filter_superblocks_usage :: BlkidProbe -> CInt -> CInt -> IO CInt

foreign import ccall "blkid_probe_enable_topology"
  probe_enable_topology :: BlkidProbe -> CInt -> IO CInt

foreign import ccall "blkid_probe_get_topology"
  probe_get_topology :: BlkidProbe -> IO BlkidTopology

foreign import ccall "blkid_topology_get_alignment_offset"
  topology_get_alignment_offset :: BlkidTopology -> IO CUInt

foreign import ccall "blkid_topology_get_minimum_io_size"
  topology_get_minimum_io_size :: BlkidTopology -> IO CUInt

foreign import ccall "blkid_topology_get_optimal_io_size"
  topology_get_optimal_io_size :: BlkidTopology -> IO CUInt

foreign import ccall "blkid_topology_get_logical_sector_size"
  topology_get_logical_sector_size :: BlkidTopology -> IO CUInt

foreign import ccall "blkid_topology_get_physical_sector_size"
  topology_get_physical_sector_size :: BlkidTopology -> IO CUInt

foreign import ccall "blkid_known_pttype" known_pttype :: CString -> IO CInt

foreign import ccall "blkid_probe_enable_partitions"
  probe_enable_partitions :: BlkidProbe -> CInt -> IO CInt

foreign import ccall "blkid_probe_reset_partitions_filter"
  probe_reset_partitions_filter :: BlkidProbe -> IO CInt

foreign import ccall "blkid_probe_invert_partitions_filter"
  probe_invert_partitions_filter :: BlkidProbe -> IO CInt

foreign import ccall "blkid_probe_filter_partitions_type"
  probe_filter_partitions_type :: BlkidProbe -> CInt -> Ptr CString -> IO CInt

foreign import ccall "blkid_probe_set_partitions_flags"
  probe_set_partitions_flags :: BlkidProbe -> CInt -> IO CInt

foreign import ccall "blkid_probe_get_partitions"
  probe_get_partitions :: BlkidProbe -> IO BlkidPartlist

foreign import ccall "blkid_partlist_numof_partitions"
  partlist_numof_partitions :: BlkidPartlist -> IO CInt

foreign import ccall "blkid_partlist_get_table"
  partlist_get_table :: BlkidPartlist -> IO BlkidParttable

foreign import ccall "blkid_partlist_get_partition"
  partlist_get_partition :: BlkidPartlist -> CInt -> IO BlkidPartition

foreign import ccall "blkid_partlist_devno_to_partition"
  partlist_devno_to_partition :: BlkidPartlist -> CDev -> IO BlkidPartition

foreign import ccall "blkid_partition_get_table"
  partition_get_table :: BlkidPartition -> IO BlkidParttable

foreign import ccall "blkid_partition_get_name"
  partition_get_name :: BlkidPartition -> IO CString

foreign import ccall "blkid_partition_get_uuid"
  partition_get_uuid :: BlkidPartition -> IO CString

foreign import ccall "blkid_partition_get_partno"
  partition_get_partno :: BlkidPartition -> IO CInt

foreign import ccall "blkid_partition_get_start"
  partition_get_start :: BlkidPartition -> IO BlkidLoff

foreign import ccall "blkid_partition_get_size"
  partition_get_size :: BlkidPartition -> IO BlkidLoff

foreign import ccall "blkid_partition_get_type"
  partition_get_type :: BlkidPartition -> IO CInt

foreign import ccall "blkid_partition_get_type_string"
  partition_get_type_string :: BlkidPartition -> IO CString

foreign import ccall "blkid_partition_get_flags"
  partition_get_flags :: BlkidPartition -> IO CULLong

foreign import ccall "blkid_partition_is_logical"
  partition_is_logical :: BlkidPartition -> IO CInt

foreign import ccall "blkid_partition_is_extended"
  partition_is_extended :: BlkidPartition -> IO CInt

foreign import ccall "blkid_partition_is_primary"
  partition_is_primary :: BlkidPartition -> IO CInt

foreign import ccall "blkid_parttable_get_type"
  parttable_get_type :: BlkidParttable -> IO CString

foreign import ccall "blkid_parttable_get_id"
  parttable_get_id :: BlkidParttable -> IO CString

foreign import ccall "blkid_parttable_get_offset"
  parttable_get_offset :: BlkidParttable -> IO BlkidLoff

foreign import ccall "blkid_parttable_get_parent"
  parttable_get_parent :: BlkidParttable -> IO BlkidPartition

foreign import ccall "blkid_do_probe" do_probe :: BlkidProbe -> IO CInt

foreign import ccall "blkid_do_safeprobe" do_safeprobe :: BlkidProbe -> IO CInt

foreign import ccall "blkid_do_fullprobe" do_fullprobe :: BlkidProbe -> IO CInt

foreign import ccall "blkid_probe_numof_values"
  probe_numof_values :: BlkidProbe -> IO CInt

foreign import ccall "blkid_probe_get_value" probe_get_value :: BlkidProbe
                                                             -> CInt
                                                             -> Ptr CString
                                                             -> Ptr CString
                                                             -> Ptr CSize
                                                             -> IO CInt

foreign import ccall "blkid_probe_lookup_value" probe_lookup_value :: BlkidProbe
                                                                   -> CString
                                                                   -> Ptr CString
                                                                   -> Ptr CSize
                                                                   -> IO CInt

foreign import ccall "blkid_probe_has_value" probe_has_value :: BlkidProbe
                                                             -> CString
                                                             -> IO CInt

foreign import ccall "blkid_do_wipe" do_wipe :: BlkidProbe -> CInt -> IO CInt

foreign import ccall "blkid_probe_step_back"
  probe_step_back :: BlkidProbe -> IO CInt

