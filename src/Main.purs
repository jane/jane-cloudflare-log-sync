module Main where

import Prelude (Unit, bind, (<<<), class Show, show, unit, pure, (<>), const, ($), (<$>), (==), (>=), (<), (&&))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
-- import Data.Either (Either(Left, Right))
import Data.Foreign.Lens (get, string, prop, json, int)
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.String (indexOf)
import Node.Encoding (Encoding(UTF8))
import Node.Process (stdin, stdout)
import Node.ReadLine (READLINE, Interface, createInterface, setLineHandler)
import Node.Stream (Readable, Write, Duplex, pipe, writeString)

foreign import data GZIP :: !

foreign import gunzip :: forall eff. Eff (gzip :: GZIP | eff) (Duplex (gzip :: GZIP | eff))

main :: forall eff. Eff (gzip :: GZIP, readline :: READLINE, console :: CONSOLE, err :: EXCEPTION | eff) Interface
main = do
  unzipperer <- gunzip
  stdin `pipe` unzipperer
  lines <- createLineReader unzipperer
  lines `setLineHandler` processLog
    where
      createLineReader :: Readable (write :: Write) (gzip :: GZIP, readline :: READLINE, console :: CONSOLE, err :: EXCEPTION | eff)
                       -> Eff (gzip :: GZIP, readline :: READLINE, console :: CONSOLE, err :: EXCEPTION | eff) Interface
      createLineReader stream = createInterface
        { input:       stream
        , output:      Nothing
        , completer:   Nothing
        , terminal:    Nothing
        , historySize: Nothing
        }

processLog :: forall eff. String -> Eff (console :: CONSOLE, err :: EXCEPTION | eff) Unit
processLog log =
  case shouldIncludeLog log of
       true  -> writeStdoutLine $ show $ getUri log
       false -> pure unit
    where
      writeStdoutLine line = (const unit) <$> writeString stdout UTF8 (line <> "\n") (pure unit)

shouldIncludeLog :: String -> Boolean
shouldIncludeLog = isSuccessStatusCode <<< getStatusCode

data StatusCode = Success Int
                | Failure Int
                | UnknownStatusCode

data Uri = Web String
         | Cdn String
         | UnknownUri

instance showUri :: Show Uri where
  show (Web uri) = "[web] " <> uri
  show (Cdn uri) = "[cdn] " <> uri
  show _         = "[bad] -- failed to find uri"

isSuccessStatusCode :: StatusCode -> Boolean
isSuccessStatusCode (Success _) = true
isSuccessStatusCode _           = false

getStatusCode :: String -> StatusCode
getStatusCode log = fromMaybe UnknownStatusCode $ getStatusCode' <$> statusCodeIntFromLog log
  where
    statusCodeIntFromLog = get (json
                       <<< prop "edgeResponse"
                       <<< prop "status"
                       <<< int)

    getStatusCode' :: Int -> StatusCode
    getStatusCode' 0                       = UnknownStatusCode
    getStatusCode' x | x >= 200 && x < 400 = Success x
    getStatusCode' x                       = Failure x

getUri :: String -> Uri
getUri log = fromMaybe UnknownUri $ getUri' <$> uriStringFromLog log
  where
    uriStringFromLog = get (json
                   <<< prop "clientRequest"
                   <<< prop "uri"
                   <<< string)

    getUri' uri | startsWith "/cdn.jane/" uri = Cdn uri
    getUri' uri                               = Web uri

startsWith :: String -> String -> Boolean
startsWith x y = fromMaybe false $ (== 0) <$> indexOf x y
