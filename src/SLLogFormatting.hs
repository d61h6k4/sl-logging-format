{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module SLLogFormatting
       (logSL, Severity(..), logSLINFO, logSLDEBUG, logSLERROR,
        renderSLLog, SLMessage)
       where


import Data.ByteString.Lazy (ByteString)
import Control.Monad.IO.Class (MonadIO)
import Data.Time
       (UTCTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode)
import Control.Monad.Log
       (WithTimestamp(..), WithSeverity(..), Severity(..), MonadLog)

import qualified Control.Monad.Log as Log

type SLMessage a = WithTimestamp (WithSeverity a)

logSL
    :: (ToJSON a, MonadIO m, MonadLog (SLMessage a) m)
    => UTCTime -> Severity -> a -> m ()
logSL timestamp' severity' msg =
    Log.logMessage
        WithTimestamp
         { discardTimestamp = WithSeverity
                               { discardSeverity = msg
                               , msgSeverity = severity'
                               }
         , msgTimestamp = timestamp'
         }


logSLSeverity
    :: (ToJSON a, MonadIO m, MonadLog (SLMessage a) m)
    => Severity -> a -> m ()
logSLSeverity severity' msg =
    Log.timestamp
        WithSeverity
         { discardSeverity = msg
         , msgSeverity = severity'
         } >>=
    Log.logMessage


logSLINFO
    :: (ToJSON a, MonadIO m, MonadLog (SLMessage a) m)
    => a -> m ()
logSLINFO = logSLSeverity Informational


logSLDEBUG
    :: (ToJSON a, MonadIO m, MonadLog (SLMessage a) m)
    => a -> m ()
logSLDEBUG = logSLSeverity Debug


logSLERROR
    :: (ToJSON a, MonadIO m, MonadLog (SLMessage a) m)
    => a -> m ()
logSLERROR = logSLSeverity Error

data SLLogLine a = SLLogLine
    { severity :: String
    , timestamp :: String
    , logMessage :: a
    } deriving (Generic)

instance ToJSON a => ToJSON (SLLogLine a)

renderSLLog :: (ToJSON a) => WithTimestamp (WithSeverity a) -> ByteString
renderSLLog msg =
    encode
        SLLogLine
        { severity = severity2nsseverity (msgSeverity (discardTimestamp msg))
        , timestamp = utc2unixtimestamp (msgTimestamp msg)
        , logMessage = discardSeverity (discardTimestamp msg)
        }

utc2unixtimestamp :: UTCTime -> String
utc2unixtimestamp = formatTime defaultTimeLocale "%s"


severity2nsseverity :: Severity -> String
severity2nsseverity sevrty =
    case sevrty of
        Informational -> "INFO"
        Debug -> "DEBUG"
        Error -> "ERROR"
        _ -> show sevrty
