{-# LANGUAGE RankNTypes #-}
module Sound.Driver.Jack where

import           Data.Word
import           Data.Bits
import           Data.Maybe
import           Foreign.Storable
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import           Foreign.C.String
import           Control.Exception (finally)
import           Text.Printf
import           System.Posix.Signals
import           Control.Concurrent.MVar
import           Sound.Types
import           Data.Stream (Stream)
import qualified Data.Stream as S
import           GHC.Float

#include<jack/jack.h>
#include<jack/thread.h>

{-runAudio = undefined-}

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
type Channels = Int

instance Show AudioFormat where
  show (AudioFormat numFormat sampleBitSize channels) =
    printf "%d bit %s %s audio" sampleBitSize (show numFormat) (showChannels channels)

showChannels :: Channels -> String
showChannels 1 = "mono"
showChannels 2 = "stereo"
showChannels _ = error "unknown channels"

instance Show NumberFormat where
  show SignedInteger = "signed integer"
  show UnsignedInteger = "unsigned integer"
  show Float = "float"

defaultAudioFormat :: AudioFormat
defaultAudioFormat = AudioFormat Float 32 1

data PortStruct
newtype Port a = Port (Ptr PortStruct)

foreign import ccall "jack/jack.h" jack_port_register ::
  JackClient -> CString -> CString -> CULong -> CULong -> IO (Port a)

foreign import ccall "jack/jack.h" jack_port_unregister ::
  JackClient -> Port a -> IO ()

data Input
data Output

registerInputPort :: JackClient -> String -> Int -> IO (Maybe (Port Input))
registerInputPort = registerPort defaultAudioFormat #{const JackPortIsInput}

registerOutputPort :: JackClient -> String -> Int -> IO (Maybe (Port Output))
registerOutputPort = registerPort defaultAudioFormat #{const JackPortIsOutput}

port :: Functor f => (forall a. Ptr a -> f (Ptr a)) -> Port b -> f (Port b)
port f (Port ptr) = Port <$> f ptr

isNullPtr :: Ptr a -> Maybe (Ptr a)
isNullPtr ptr
  | ptr == nullPtr = Nothing
  | otherwise      = Just ptr

type PortFlags = CULong
registerPort :: AudioFormat -> PortFlags -> JackClient -> String -> Int -> IO (Maybe (Port a))
registerPort audioFormat flags client portName bufSize =
  withCString portName $ \cportName ->
    withCString (show audioFormat) $ \caudioFormat ->
      port isNullPtr <$>
        jack_port_register client cportName caudioFormat flags (fromIntegral bufSize)

unregisterPort :: JackClient -> Port a -> IO ()
unregisterPort = jack_port_unregister

withInputPort :: JackClient -> String -> Int -> (Port Input -> IO ()) -> IO ()
withInputPort = withPort registerInputPort

withOutputPort :: JackClient -> String -> Int -> (Port Output -> IO ()) -> IO ()
withOutputPort = withPort registerOutputPort

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

data TransportState =
  Stopped | Rolling | Starting | NetStarting
  deriving (Show,Eq)

data PositionStruct
data Position = Position
  { bar :: Int
  , beat :: Int
  , tick :: Int
  , barStartTick :: Double
  , beatPerBar :: Double
  , beatType :: Double
  , ticksPerMinute :: Double
  , beatsPerMinute :: Double
  , frameTime :: Double
  , nextTime :: Double
  } deriving (Show,Eq)

type SyncCallback = FunPtr (CInt -> Ptr PositionStruct -> IO CInt)
foreign import ccall "wrapper"
  mkSyncCallback :: (CInt -> Ptr PositionStruct -> IO CInt) -> IO SyncCallback

foreign import ccall "jack/transport.h" jack_set_sync_callback ::
  JackClient -> SyncCallback -> Ptr () -> IO ()

data ReadyToRoll = ReadyToRoll | NotReady

jackSync :: JackClient -> (TransportState -> Position -> IO ReadyToRoll) -> IO ()
jackSync client syncCallback = do
  cb <- mkSyncCallback $ \trans pos -> do
    pos' <- peekJackPosition pos
    encodeStatus <$> syncCallback (decodeTransport trans) pos'
    
  jack_set_sync_callback client cb nullPtr
      
  where
    decodeTransport :: CInt -> TransportState
    decodeTransport t = case t of
      #{const JackTransportStopped}     -> Stopped
      #{const JackTransportRolling}     -> Rolling
      #{const JackTransportStarting}    -> Starting
      #{const JackTransportNetStarting} -> NetStarting
      _                                 -> error ("deconding jack_transport_state_t: code " ++ show t)

    encodeStatus :: ReadyToRoll -> CInt
    encodeStatus ReadyToRoll = 1
    encodeStatus NotReady    = 0

    peekJackPosition :: Ptr PositionStruct -> IO Position
    peekJackPosition ptr =
      Position
        <$> #{peek jack_position_t,bar} ptr
        <*> #{peek jack_position_t,beat} ptr
        <*> #{peek jack_position_t,tick} ptr
        <*> #{peek jack_position_t,bar_start_tick} ptr
        <*> #{peek jack_position_t,beats_per_bar} ptr
        <*> #{peek jack_position_t,beat_type} ptr
        <*> #{peek jack_position_t,ticks_per_beat} ptr
        <*> #{peek jack_position_t,beats_per_minute} ptr
        <*> #{peek jack_position_t,frame_time} ptr
        <*> #{peek jack_position_t,next_time} ptr
  

jackConnect :: JackClient -> String -> String -> IO ()
jackConnect client from to =
  withCString from $ \cfrom ->
  withCString to $ \cto ->
    jack_connect client cfrom cto

runAudio :: Audio -> IO ()
runAudio audio = do
  sig <- newMVar $ double2CFloat <$> audio
  withClient "hsynth" $ \client -> do
    withOutputPort client "out" 0 $ \output -> do
      jackProcess client $ \nframes -> do
        buf <- jack_port_get_buffer output nframes
        modifyMVar_ sig $ store buf nframes

      jackSync client $ \state pos -> do
        print (state,pos)
        return ReadyToRoll

      jack_activate client

      jackConnect client "hsynth:out" "system:playback_1"
      jackConnect client "hsynth:out" "system:playback_2"

      putStrLn "Started"

      awaitSigint

      putStrLn "stopped"

      return ()

double2CFloat :: Double -> CFloat
double2CFloat x = CFloat (double2Float x)
{-# INLINE double2CFloat #-}

awaitSigint :: IO ()
awaitSigint = do
    sig <- newEmptyMVar
    _ <- installHandler keyboardSignal (Catch $ putMVar sig ()) Nothing
    takeMVar sig

store :: Ptr CFloat -> NFrames -> Stream CFloat -> IO (Stream CFloat)
store buf len = go 0
  where
    go :: Int -> Stream CFloat -> IO (Stream CFloat)
    go i s@(S.Cons x xs)
      | i >= fromIntegral len = return s
      | otherwise = do
        pokeElemOff buf i x
        go (i+1) xs

{-
import           Control.Concurrent.MVar
import           Control.Monad.Trans
import           Control.Monad.Exception.Synchronous

import           Data.Array.Storable (writeArray)
import qualified Data.ByteString.Lazy as B
import           Data.Vector.Primitive (Vector)
import qualified Data.Vector.Primitive as V

import           Foreign.C.Types (CFloat(..))
import           Foreign.C.Error(Errno)

import           GHC.Float

import           Sound.JACK (NFrames(..),Output,Input)
import           Sound.JACK.Audio (Port)
import qualified Sound.JACK.Audio as Audio
import qualified Sound.JACK as JACK
import qualified Sound.JACK.MIDI as MIDI
import           Sound.Sample
import           Sound.Types

import qualified Data.Stream as S

import           Music.Midi
import           Music.VoiceMap (VoiceMap)
import qualified Music.VoiceMap as VM

type Synthesizer = Pitch -> Velocity -> Rate -> Sample

runAudio :: Synthesizer -> IO ()
runAudio synth = do
  sig <- newMVar VM.empty
  start "hsynth" synth sig

start :: String -> Synthesizer -> MVar VoiceMap -> IO ()
start name synth vm = do
  JACK.handleExceptions $
      JACK.withClientDefault name $ \client ->
      MIDI.withPort client "input" $ \input ->
      JACK.withPort client "output" $ \output -> do
        rate <- lift $ JACK.getSampleRate client
        JACK.withProcess client (render synth vm input output rate) $
            JACK.withActivation client $ lift $ do
                putStrLn $ "started " ++ name ++ "..."
                JACK.waitForBreak

render :: Synthesizer -> MVar VoiceMap -> MIDI.Port Input -> Port Output -> Rate -> NFrames -> ExceptionalT Errno IO ()
render synth mvm input output rate nframes@(NFrames n) = do
  lift $ print "called"
  rawMidiEvents <- MIDI.readRawEventsFromPort input nframes
  let midiMsgs = concat $ map (getMessages . B.fromStrict . MIDI.rawEventBuffer) rawMidiEvents
  lift $ do
    printMidiMsg $ filterMidiMsg midiMsgs
    out <- Audio.getBufferArray output nframes
    modifyMVar_ mvm $ \vm -> do
      let vm' = foldr (VM.interpret (\pitch vel -> synth pitch vel rate)) vm midiMsgs
      let (sample,vm'') = VM.mapAccumNotes mixSample mixSampleList emptySample vm'
      write out 0 (V.toList sample)
      return vm''
    write out 0 (repeat 0 :: [Double])
  where
    emptySample = V.replicate (fromIntegral n) 0

    filterMidiMsg :: [MidiMessage] -> [MidiMessage]
    filterMidiMsg = filter (\msg -> case msg of SystemRealTime _ -> False; _ -> True)

    printMidiMsg :: [MidiMessage] -> IO ()
    printMidiMsg [] = return ()
    printMidiMsg l = print l

    mixSample :: Vector Double -> Audio -> (Vector Double, Audio)
    mixSample sample audio =
      let (sample',audio') = S.splitAt (fromIntegral n) audio
      in (V.zipWith (+) sample (V.fromList sample'),audio')

    mixSampleList :: Vector Double -> [Double] -> (Vector Double, [Double])
    mixSampleList sample audio =
      let (sample',audio') = splitAt (fromIntegral n) audio
      in (V.zipWith (+) sample (V.fromList sample'),audio')

    write out i (amp:samp) | i < n = do
      writeArray out (NFrames i) (double2CFloat amp)
      write out (i+1) samp
    write _ _ _ = return ()

double2CFloat :: Double -> CFloat
double2CFloat x = CFloat (double2Float x)
{-# INLINE double2CFloat #-}
-}
