{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad ((>=>),forever,when)
import           Data.Foldable(traverse_)
import           Data.Text (Text,unpack)
import           Data.Map (Map)
import qualified Data.Map as M
import           System.Process
import           System.Environment (getArgs)
import qualified DBus as DBus
import qualified DBus.Client as DBus
import           System.Exit
import           System.IO
import           System.Directory(makeAbsolute)
import           Text.Printf
import           Data.Maybe(fromJust)
import           Data.String(fromString)

data Server = Server (Map Text ProcessHandle)

data Method
  = AddNode
  | RemoveNode
  | ReplaceNode
  deriving Show

method :: Method -> DBus.MemberName
method = fromString . show

interface :: DBus.InterfaceName
interface = "org.hsynth.server"

object :: DBus.ObjectPath
object = "/hsynth"

addNode :: Text -> FilePath -> Server -> IO Server
addNode name path (Server procs) = do
  printf "org.hynth.server.AddNode(%s,%s)\n" (unpack name) path
  (_,_,_,handle) <- createProcess (proc path [])
  return $ Server $ M.insert name handle procs

removeNode :: Text -> Server -> IO Server
removeNode name (Server procs) = do
  printf "org.hynth.server.RemoveNode(%s)\n" (unpack name)
  maybe (return ()) terminateProcess (M.lookup name procs)
  return $ Server $ M.delete name procs

replaceNode :: Text -> FilePath -> Server -> IO Server
replaceNode name path = removeNode name >=> addNode name path

usage :: String
usage =
  "hsynth-control server\n" ++
  "hsynth-control play [node] [path]\n" ++
  "hsynth-control stop [node]\n" ++
  "hsynth-control replace [node] [path]\n"

main = do
  args <- getArgs
  case args of
    ["server"]    -> startServer
    ["stop",node] -> callMethod RemoveNode [DBus.toVariant node]
    [m,node,path] -> do
      path' <- makeAbsolute path
      case m of
        "play"    -> callMethod AddNode     (DBus.toVariant <$> [node,path'])
        "replace" -> callMethod ReplaceNode (DBus.toVariant <$> [node,path'])
    _ -> die usage

callMethod :: Method -> [DBus.Variant] -> IO ()
callMethod m args = do
  client <- DBus.connectSession

  e <- DBus.call client (DBus.methodCall object interface (method m))
    { DBus.methodCallDestination = Just "org.hsynth"
    , DBus.methodCallBody = args
    }

  case e of
    Left err -> putStrLn $ fromJust $ DBus.fromVariant $ head $ DBus.methodErrorBody err
    Right _  -> return ()

startServer :: IO ()
startServer = do
  server <- newMVar $ Server M.empty
  client <- DBus.connectSession
  requestResult <- DBus.requestName client "org.hsynth" []
  when (requestResult /= DBus.NamePrimaryOwner) $ do
    putStrLn "Another service owns the \"org.hsynth\" bus name"
    exitFailure

  DBus.export client object
    [ DBus.autoMethod interface (method AddNode)     (\t f -> (modifyMVar_ server (addNode t f)))
    , DBus.autoMethod interface (method RemoveNode)  (\t   -> (modifyMVar_ server (removeNode t)))
    , DBus.autoMethod interface (method ReplaceNode) (\t f -> (modifyMVar_ server (replaceNode t f)))
    ]

  forever (threadDelay 50000)
