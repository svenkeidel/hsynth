{-# LANGUAGE RankNTypes #-}
module Sound.Driver.Jack where

import           Control.Concurrent.MVar
import           Control.Exception (finally)
import           Control.Monad(forM)

import qualified Data.Binary.Get as G
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as B
import           Data.Maybe
import           Data.Stream (Reactive,Streamable)
import qualified Data.Stream as S
import           Data.Word

import           Foreign.Storable
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import           Foreign.C.String

import           GHC.Float

import           Music.Midi

import           System.Posix.Signals

import           Sound.Types

import           Text.Printf

#include<jack/jack.h>
#include<jack/midiport.h>
#include<jack/thread.h>

data JackClientStruct
type JackClient = Ptr JackClientStruct

data JackOption = NoStartServer | UseExactName | ServerName | LoadName | LoadInit | SessionID
  deriving (Show,Eq)
data JackStatus = Failure | InvalidOption | NameNotUnique | ServerStarted
  | ServerFailed | ServerError | NoSuchClient | LoadFailure | InitFailure
  | ShmFailure | VersionError | BackendError | ClientZombie
  deriving (Show,Eq)

foreign import ccall "jack/jack.h" jack_client_open :: CString -> #{type jack_options_t} -> Ptr #{type jack_status_t} -> IO JackClient

openClient :: String -> [JackOption] -> IO (Either [JackStatus] JackClient)
openClient name options = do
  withCString name $ \cname ->
    alloca $ \statusPtr -> do
      jackClient <- jack_client_open cname (encodeOptions options) statusPtr
      status <- decodeStatus <$> peek statusPtr
      return $ if not (null status)
                then Left status
                else Right jackClient
  where
    encodeOptions :: [JackOption] -> #{type jack_options_t}
    encodeOptions = foldr (\option -> (encodeOption option .|.)) #{const JackNullOption}

    encodeOption :: JackOption -> #{type jack_options_t}
    encodeOption option =
      case option of
        NoStartServer -> #{const JackNoStartServer}
        UseExactName  -> #{const JackUseExactName}
        ServerName    -> #{const JackServerName}
        LoadName      -> #{const JackLoadName}
        LoadInit      -> #{const JackLoadInit}
        SessionID     -> #{const JackSessionID}

    decodeStatus :: #{type jack_status_t} -> [JackStatus]
    decodeStatus status = catMaybes
      [ present Failure #{const JackFailure}
      , present InvalidOption #{const JackInvalidOption}
      , present NameNotUnique #{const JackNameNotUnique}
      , present ServerStarted #{const JackServerStarted}
      , present ServerFailed #{const JackServerFailed}
      , present ServerError #{const JackServerError}
      , present NoSuchClient #{const JackNoSuchClient}
      , present LoadFailure #{const JackLoadFailure}
      , present InitFailure #{const JackInitFailure}
      , present ShmFailure #{const JackShmFailure}
      , present VersionError #{const JackVersionError}
      , present BackendError #{const JackBackendError}
      , present ClientZombie #{const JackClientZombie}
      ]
      where
        present :: JackStatus -> #{type jack_status_t} -> Maybe JackStatus
        present st flag
          | status .&. flag /= 0 = Just st
          | otherwise            = Nothing


foreign import ccall "jack/jack.h" jack_client_close :: JackClient -> IO ()

closeClient :: JackClient -> IO ()
closeClient = jack_client_close

withClient :: String -> (JackClient -> IO ()) -> IO ()
withClient name handle = do
  c <- openClient name []
  case c of
    Left status -> print status
    Right client -> handle client
      `finally` (closeClient client)

data AudioFormat = AudioFormat NumberFormat SampleBitSize Channels
data NumberFormat = SignedInteger | UnsignedInteger | Float
type SampleBitSize = Int
data Channels = Mono | Stereo

instance Show AudioFormat where
  show (AudioFormat numFormat sampleBitSize channels) =
    printf "%d bit %s %s audio" sampleBitSize (show numFormat) (show channels)

instance Show Channels where
  show Mono = "mono"
  show Stereo = "stereo"

instance Show NumberFormat where
  show SignedInteger = "signed integer"
  show UnsignedInteger = "unsigned integer"
  show Float = "float"

defaultAudioFormat :: AudioFormat
defaultAudioFormat = AudioFormat Float 32 Mono

data PortStruct
newtype Port a = Port (Ptr PortStruct)

foreign import ccall "jack/jack.h" jack_port_register ::
  JackClient -> CString -> CString -> CULong -> CULong -> IO (Port a)

foreign import ccall "jack/jack.h" jack_port_unregister ::
  JackClient -> Port a -> IO ()

data Input

registerInputPort :: JackClient -> String -> Int -> IO (Maybe (Port Input))
registerInputPort = registerPort (show defaultAudioFormat) #{const JackPortIsInput}

data Output

registerOutputPort :: JackClient -> String -> Int -> IO (Maybe (Port Output))
registerOutputPort = registerPort (show defaultAudioFormat) #{const JackPortIsOutput}

data MidiInput

registerMidiInputPort :: JackClient -> String -> Int -> IO (Maybe (Port MidiInput))
registerMidiInputPort = registerPort defaultMidiFormat #{const JackPortIsInput}

defaultMidiFormat :: String
defaultMidiFormat = "8 bit raw midi"

port :: Functor f => (forall a. Ptr a -> f (Ptr a)) -> Port b -> f (Port b)
port f (Port ptr) = Port <$> f ptr

isNullPtr :: Ptr a -> Maybe (Ptr a)
isNullPtr ptr
  | ptr == nullPtr = Nothing
  | otherwise      = Just ptr

type PortFlags = CULong
registerPort :: String -> PortFlags -> JackClient -> String -> Int -> IO (Maybe (Port a))
registerPort format flags client portName bufSize =
  withCString portName $ \cportName ->
    withCString format $ \caudioFormat ->
      port isNullPtr <$>
        jack_port_register client cportName caudioFormat flags (fromIntegral bufSize)

unregisterPort :: JackClient -> Port a -> IO ()
unregisterPort = jack_port_unregister

withInputPort :: JackClient -> String -> Int -> (Port Input -> IO ()) -> IO ()
withInputPort = withPort registerInputPort

withOutputPort :: JackClient -> String -> Int -> (Port Output -> IO ()) -> IO ()
withOutputPort = withPort registerOutputPort

withMidiInputPort :: JackClient -> String -> Int -> (Port MidiInput -> IO ()) -> IO ()
withMidiInputPort = withPort registerMidiInputPort

withPort :: (JackClient -> String -> Int -> IO (Maybe (Port a)))
          -> JackClient -> String -> Int -> (Port a -> IO ()) -> IO ()
withPort register client portName bufSize handler = do
  mp <- register client portName bufSize
  case mp of
    Nothing -> putStrLn "Cannot open port"
    Just p ->
      handler p `finally` unregisterPort client p


type NFrames = #{type jack_nframes_t}

type ProcessCallback = FunPtr (NFrames -> Ptr () -> IO CInt)
foreign import ccall "wrapper"
  mkProcessCallback :: (NFrames -> Ptr () -> IO CInt) -> IO ProcessCallback

jackProcess :: JackClient -> (NFrames -> IO ()) -> IO ()
jackProcess client process = do
  cb <- mkProcessCallback (\frames _ -> process (fromIntegral frames) >> return 0)
  jack_set_process_callback client cb nullPtr

foreign import ccall "jack/jack.h" jack_set_process_callback ::
  JackClient -> ProcessCallback -> Ptr () -> IO ()

foreign import ccall "jack/jack.h" jack_activate ::
  JackClient -> IO ()

foreign import ccall "jack/jack.h" jack_port_get_buffer ::
  Port a -> NFrames -> IO (Ptr b)

foreign import ccall "jack/jack.h" jack_get_buffer_size ::
  JackClient -> IO (Ptr b)

foreign import ccall "jack/jack.h" jack_get_sample_rate ::
  JackClient -> IO NFrames

foreign import ccall "jack/jack.h" jack_connect ::
  JackClient -> CString -> CString -> IO ()

foreign import ccall "jack/midiport.h" jack_midi_get_event_count ::
  Ptr a -> IO CUInt

foreign import ccall "jack/midiport.h" jack_midi_event_get ::
  Ptr MidiEvent -> Ptr a -> CUInt -> IO CInt

data MidiEventStruct
data MidiEvent = MidiEvent
  { time   :: NFrames
  , size   :: CSize
  , buffer :: Ptr CChar
  }

getMidiEvents :: Port MidiInput -> NFrames -> IO [MidiMessage]
getMidiEvents midiPort nframes = do
  buf <- jack_port_get_buffer midiPort nframes
  cnt <- jack_midi_get_event_count buf
  allocaBytes #{size jack_midi_event_t} $ \eventPtr ->
    fmap catMaybes $ forM [0..cnt] $ \i -> do
      valid <- jack_midi_event_get eventPtr buf i
      s <- #{peek jack_midi_event_t,size} eventPtr :: IO CSize
      b <- #{peek jack_midi_event_t,buffer} eventPtr :: IO (Ptr Word8)
      if b == nullPtr || valid /= 0
        then return Nothing
        else do
          bs <- B.unsafePackCStringFinalizer b (fromIntegral s) (return ())
          let msg = Just $ G.runGet getMessage $ BL.fromStrict bs
          print msg
          return msg

jackConnect :: JackClient -> String -> String -> IO ()
jackConnect client from to =
  withCString from $ \cfrom ->
  withCString to $ \cto ->
    jack_connect client cfrom cto

runAudioWithMidi :: Reactive MidiMessage Double -> IO ()
runAudioWithMidi r0 = do
  sig <- newMVar r0
  withClient "hsynth" $ \client -> do
    withOutputPort client "out" 0 $ \output -> do
      withMidiInputPort client "midi_in" 0 $ \midiIn -> do
        jackProcess client $ \nframes -> do
          evnts <- getMidiEvents midiIn nframes
          buf <- jack_port_get_buffer output nframes
          modifyMVar_ sig $ store buf nframes . S.reacting evnts
        jackStartup client

runAudio :: Audio -> IO ()
runAudio audio = do
  sig <- newMVar audio
  withClient "hsynth" $ \client -> do
    withOutputPort client "out" 0 $ \output -> do
      jackProcess client $ \nframes -> do
        buf <- jack_port_get_buffer output nframes
        modifyMVar_ sig $ store buf nframes

      jackStartup client

jackStartup :: JackClient -> IO ()
jackStartup client = do
  jack_activate client

  jackConnect client "hsynth:out" "system:playback_1"
  jackConnect client "hsynth:out" "system:playback_2"

  putStrLn "started"

  awaitSigint

  putStrLn "stopped"

double2CFloat :: Double -> CFloat
double2CFloat x = CFloat (double2Float x)
{-# INLINE double2CFloat #-}

awaitSigint :: IO ()
awaitSigint = do
  sig <- newEmptyMVar
  _ <- installHandler keyboardSignal (Catch $ putMVar sig ()) Nothing
  takeMVar sig

store :: Streamable s => Ptr CFloat -> NFrames -> s Double -> IO (s Double)
store buf len = go 0
  where
    go i s
      | i >= fromIntegral len = return s
      | otherwise = do
        pokeElemOff buf i (double2CFloat (S.head s))
        go (i+1) (S.tail s)
