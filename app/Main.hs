module Main where

import Options (appOpts, Ports (..))
import Options.Applicative (execParser)


main :: IO ()
main = do
  Ports websocketPort zeromqPort <- execParser appOpts

  print (websocketPort, zeromqPort)
