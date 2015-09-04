{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Network
import           Options.Applicative

data Options = Options
    { optionsPort      :: Maybe Int
    , optionsMultiLine :: Bool
    }

main :: IO ()
main = do
    Options port multiline <- execParser opts
    cmd <- T.getLine
    client cmd (PortNumber . fromIntegral $ fromMaybe 4242 port) >>= (putStrLn . unescapeIf multiline . T.unpack)
  where
    parser = Options <$> optional (option auto (long "port" <> short 'p'))
                     <*> switch (long "multiline"
                              <> help "Multiline output instead of escaped newlines.")
    opts = info parser mempty

    unescapeIf :: Bool -> String -> String
    unescapeIf b = if b then read else id

client :: T.Text -> PortID -> IO T.Text
client cmd port = do
  h <- connectTo "localhost" port
  T.hPutStrLn h cmd
  T.hGetLine h
