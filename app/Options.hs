module Options where

import Options.Applicative
  ( option, auto, Parser, long, short, metavar, showDefault, help, value, info, fullDesc, progDesc
  , ParserInfo, helper, (<**>))


websocketPort :: Parser Int
websocketPort = option auto $
  long "websocket"
    <> short 'w'
    <> metavar "WS PORT"
    <> showDefault
    <> value 3000
    <> help "Binds to port WS PORT for WebSocket tests"

zeromqPort :: Parser Int
zeromqPort = option auto $
  long "zeromq"
    <> short 'z'
    <> metavar "ZMQ PORT"
    <> showDefault
    <> value 3001
    <> help "Binds to port ZMQ PORT for ZeroMQ tests"


data Ports = Ports
  { websocketPort' :: Int
  , zeromqPort' :: Int
  }


appOpts :: ParserInfo Ports
appOpts = info ((Ports <$> websocketPort <*> zeromqPort) <**> helper) $
  fullDesc
    <> progDesc "symbiote-server - symbiote implementation for the symbiotic-data standard."
