module Main where

import           Control.Concurrent       (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.Async (async)
import           Control.Monad            (forM_, when)
import           Control.Monad.Catch      (catchAll, finally)
import           Data.Either.Extra        (mapLeft)
import           Data.IP                  (IP)
import           Data.List                (sort)
import           Network.Simple.TCP       (connect)
import           System.Environment       (getArgs, getProgName)
import           System.Exit              (exitSuccess)
import           System.IO                (hFlush, stdout)
import           Text.Read                (readEither)


maxPort = 65535


main :: IO ()
main = do
  args    <- getArgs
  program <- getProgName
  case newArguments args of
    Left  err       -> errArgs program err >> exitSuccess
    Right Help      -> usage >> exitSuccess
    Right arguments -> scans arguments >> exitSuccess


scans args@(Arguments ipaddr threads) = do
  ch <- newChan
  forM_ [1 .. threads] $ \i -> async $ scan ch i args
  ports <- range ch
  putStrLn ""
  forM_ (sort ports) $ \p -> putStrLn $ show p ++ " is open"


scan ch port args@(Arguments ipaddr threads) =
  when (port <= maxPort)
    $ connect (show ipaddr) (show port) (\_ -> do
                  writeChan ch port
                  putStr "."
                  hFlush stdout
                )
    `catchAll` (\_ -> return ())
    `finally` scan ch (port + threads) args


data Arguments = Help | Arguments {ipaddr::IP, threads::Int}


newArguments :: [String] -> Either String Arguments
newArguments []                     = Left "not enough arguments"
newArguments args | length args > 3 = Left "too many arguments"
newArguments (h : rest) | h == "-h" || h == "-help" =
  if null rest then Right Help else Left "too many arguments"
newArguments (j : t : ip : _) | j == "-j" = do
  threads <- readOr t "failed to parse thread number"
  ipaddr  <- readOr ip "not a valid IPADDR; must be IPv4 or IPv6"
  return $ Arguments ipaddr threads
newArguments (ip : _) = do
  ipaddr <- readOr ip "not a valid IPADDR; must be IPv4 or IPv6"
  return $ Arguments ipaddr 4


errArgs program err =
  putStrLn (program ++ " problem parsing arguments: " ++ err)


usage = putStrLn "Usage: -j to select how many therads you want"
  >> putStrLn "       -h or -help to show this help message"


readOr :: Read a => String -> String -> Either String a
readOr s err = mapLeft (const err) $ readEither s


range :: Chan a -> IO [a]
range ch = do
  c  <- readChan ch
  cs <- range ch `catchAll` (\_ -> return [])
  return (c : cs)
