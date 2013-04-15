{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoMonomorphismRestriction, OverloadedStrings, TypeFamilies #-}
module Network.DGS.Monad
	( module Network.DGS.Monad
	, module Control.Monad.Error
	, module Control.Monad.RWS
	) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Error hiding (Error)
import Control.Monad.RWS hiding (get)
import Control.Monad.Trans.Control
import Data.Aeson       hiding (Success)
import Data.Aeson.Types hiding (Success)
import Data.Conduit
import Data.Time
import Network.DGS.Errors
import Network.HTTP.Conduit hiding (Response)
import Network.HTTP.Types
import Network.Socket
import System.Locale
import qualified Control.Monad.Error        as E
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text                  as T

-- | a monad stack to ease the use of http-conduit
newtype DGS a = DGS { runDGS :: DGS' a } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadUnsafeIO, MonadResource, MonadReader (Server, Manager), MonadState (CookieJar, Maybe Quota), MonadBase IO, MonadError DGSException)
type DGS' = ErrorT DGSException (RWST (Server, Manager) () (CookieJar, Maybe Quota) (ResourceT IO))
type Server = S.ByteString -- ^ e.g. 'Network.DGS.production' or 'Network.DGS.development'

-- this code is really unreadable, but it was written by just following the
-- types and applying the DGS/runDGS and StM/unStM isomorphisms as necessary
instance MonadBaseControl IO DGS where
	data StM DGS a = StM !(StM DGS' a)
	liftBaseWith f = DGS (liftBaseWith (\g -> f (\(DGS m) -> StM <$> g m)))
	restoreM (StM v) = DGS (restoreM v)

-- | a wrapper that exists entirely for its 'FromJSON' instance
newtype DGSTime = DGSTime { unDGSTime :: UTCTime } deriving (Eq, Ord, Show, Read)
instance FromJSON DGSTime where
	parseJSON (String s) = case parseTime defaultTimeLocale "%F %T" (T.unpack s) of
		Just t  -> pure (DGSTime t)
		Nothing -> fail $ "date in strange format: " ++ T.unpack s
	parseJSON v = typeMismatch "date" v

-- | how many accesses you have left, and when the quota will reset to its
-- maximum
data Quota = Quota Integer UTCTime deriving (Eq, Ord, Show, Read)

instance FromJSON Quota where
	parseJSON (Object v) = Quota
		<$> v .: "quota_count"
		<*> (unDGSTime <$> v .: "quota_expire")
	parseJSON v = typeMismatch "DGS response with quota included" v

data DGSException
	= UnknownVersion String  -- ^ currently, only 1.0.15:3 is supported
	| DGSProblem Error       -- ^ something was wrong with your request
	| NoParse L.ByteString   -- ^ the server sent this response, which was outside the format it promised to send (e.g. not valid JSON, or JSON but not in the right schema, etc.)
	| CustomException String -- ^ somebody (not this library) called 'fail'
	deriving (Eq, Ord, Show, Read)

instance E.Error DGSException where strMsg = CustomException

instance FromJSON (Maybe DGSException) where
	parseJSON v@(Object o) = do
		version <- o .: "version"
		error   <- o .: "error"
		case (version, error, label error) of
			("1.0.15:3", "", _) -> pure Nothing
			("1.0.15:3", _ , l) -> Just . DGSProblem . maybe (UnknownError error) KnownError l <$> o .: "error_msg"
			(_         , _ , _) -> pure . Just . UnknownVersion $ version
	parseJSON v = typeMismatch "DGS response with version and error included" v

-- boy, I sure do hope the path argument is ASCII
get :: String -> [(String, String)] -> DGS L.ByteString
get path query = do
	(host, manager) <- ask
	now             <- liftIO getCurrentTime
	r               <- firstState (\cookies -> insertCookiesIntoRequest (r_ host) cookies now)
	resp_           <- httpLbs r manager
	now             <- liftIO getCurrentTime
	resp            <- firstState ((\(a,b) -> (b,a)) . updateCookieJar resp_ r now)
	return $ responseBody resp
	where
	firstState f = state (\(s1, s2) -> let (a, s1') = f s1 in (a, (s1', s2)))
	r_ h = def
		{ host        = h
		, path        = S.pack ("/" <> path <> ".php")
		, queryString = renderQuery False (toQuery query)
		}

instance FromJSON a => FromJSON (Maybe Quota, Either DGSException a) where
	parseJSON v = do
		q  <- (Just <$> parseJSON v) <|> pure Nothing
		me <- parseJSON v
		case me of
			Just e  -> return (q, Left e)
			Nothing -> do
				a <- parseJSON v
				return (q, Right a)

getQuota   = gets snd
setQuota q = modify (\(cookies, quota) -> (cookies, Just q))

object obj cmd opts = do
	response <- get "quick_do" (("obj",obj):("cmd",cmd):opts)
	case decode response of
		Nothing     -> throwError (NoParse response)
		Just (q, v) -> do
			maybe (return ()) setQuota q
			either throwError return v

runRWST_ :: Monad m => s -> RWST r w s m a -> r -> m a
runRWST_ s rwst r = (\(a,_,_) -> a) `liftM` runRWST rwst r s

browseDGS :: Server -> DGS a -> IO (Either DGSException a)
browseDGS server = withSocketsDo . withManager . ($server) . curry . runRWST_ def . runErrorT . runDGS
