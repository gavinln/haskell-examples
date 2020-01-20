{-
 - http://www.haskellforall.com/2018/10/detailed-walkthrough-for-beginner.html
 - nix-shell --run 'hoogle "Text -> (Text, Text)"'
 - In ghci run this
 - :set -XOverloadedStrings
 - ghc -O2 -package safe align-equals.hs
 -}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)

import qualified Data.Text
import qualified Data.Text.IO
import qualified Safe

prefixLength :: Text -> Int
prefixLength line = Data.Text.length prefix
  where
    (prefix, suffix) = Data.Text.breakOn "=" line

adjustLine
  :: Int  -- desired prefix length
  -> Text  -- line to pad
  -> Text  -- padded line
adjustLine desiredPrefixLength oldLine = newLine
  where
    (prefix, suffix) = Data.Text.breakOn "=" oldLine
    
    actualPrefixLength = Data.Text.length prefix

    additionalSpaces = desiredPrefixLength - actualPrefixLength

    spaces = Data.Text.replicate additionalSpaces " "

    newLine = Data.Text.concat [prefix, spaces, suffix]

adjustText :: Text -> Text
adjustText oldText = newText
  where
    oldLines = Data.Text.lines oldText

    prefixLengths = map prefixLength oldLines

    newLines = 
      case Safe.maximumMay prefixLengths of
        Nothing ->
          []
        Just desiredPrefixLength ->
          map (adjustLine desiredPrefixLength) oldLines

    newText = Data.Text.unlines newLines

main :: IO ()
main = Data.Text.IO.interact adjustText

