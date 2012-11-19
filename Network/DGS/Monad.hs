{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, TypeFamilies #-}
module Network.DGS.Monad where

import Control.Applicative
import Control.Monad.Base
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
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict        as H
import qualified Data.Text                  as T

-- | a monad stack to ease the use of http-conduit
newtype DGS a = DGS { runDGS :: DGS' a } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadUnsafeIO, MonadResource, MonadReader Manager, MonadState CookieJar, MonadBase IO)
type DGS' = RWST Manager () CookieJar (ResourceT IO)

-- this code is really unreadable, but it was written by just following the
-- types and applying the DGS/runDGS and StM/unStM isomorphisms as necessary
instance MonadBaseControl IO DGS where
	data StM DGS a = StM !(StM DGS' a)
	liftBaseWith f = DGS (liftBaseWith (\g -> f (\(DGS m) -> StM <$> g m)))
	restoreM (StM v) = DGS (restoreM v)

-- | how many accesses you have left, and when the quota will reset to its
-- maximum
data Quota = Quota Integer UTCTime deriving (Eq, Ord, Show, Read)

instance FromJSON Quota where
	parseJSON (Object v) = case H.lookup "quota_expire" v of
		Just (String s) -> case parseTime defaultTimeLocale "%F %T" (T.unpack s) of
			Just t  -> Quota <$> v .: "quota_count" <*> pure t
			Nothing -> fail $ "quota expiration date in strange format: " ++ T.unpack s
		Just s  -> typeMismatch "date" s
		Nothing -> fail "quota expiration date missing"

data Response a
	= Success               Quota  a      -- ^ the stars aligned; here's your answer!
	| UnknownVersion (Maybe Quota) String -- ^ currently, only 1.0.15:2 is supported
	| Problem               Quota  Error  -- ^ something was wrong with your request
	| NoLogin                      Error  -- ^ not logged in for some reason (maybe the 'Error' has more details, but probably you unwisely ignored the return value from 'login' earlier)
	| NoParse                             -- ^ the server sent invalid JSON or valid JSON outside the schema it promised to deliver
	deriving (Eq, Ord, Show, Read)

instance FromJSON a => FromJSON (Response a) where
	parseJSON v@(Object o) = do
		quota   <- (Just <$> parseJSON v) <|> pure Nothing
		version <- o .: "version"
		error   <- o .: "error"
		case (version, error, label error, quota) of
			("1.0.15:2", _ , l, Nothing   ) -> NoLogin . maybe (UnknownError error) KnownError l <$> o .: "error_msg"
			("1.0.15:2", "", _, Just quota) -> Success quota <$> parseJSON v
			("1.0.15:2", _ , l, Just quota) -> Problem quota . maybe (UnknownError error) KnownError l <$> o .: "error_msg"
			(_         , _ , _, _         ) -> pure (UnknownVersion quota version)

-- only call this with ASCII host and path parameters, please
uri :: String -> String -> Request m
uri host_ path_ = def { host = S.pack host_, path = S.pack ('/':path_) }

form :: Method -> (L.ByteString -> a) -> Request DGS -> [(String, String)] -> DGS a
get  ::           (L.ByteString -> a) -> Request DGS -> [(String, String)] -> DGS a
post ::           (L.ByteString -> a) -> Request DGS -> [(String, String)] -> DGS a
form t f r_ q = do
	manager <- ask
	now     <- liftIO getCurrentTime
	r       <- state (\cookies -> insertCookiesIntoRequest r_ { queryString = renderQuery False (toQuery q) } cookies now)
	resp_   <- httpLbs r manager
	now     <- liftIO getCurrentTime
	resp    <- state ((\(a,b) -> (b,a)) . updateCookieJar resp_ r now)
	return . f . responseBody $ resp

get  = form methodGet
post = form methodPost
object obj cmd opts server = get
	(maybe NoParse id . decode)
	(uri server "quick_do.php")
	(("obj",obj):("cmd",cmd):opts)

runRWST_ :: Monad m => s -> RWST r w s m a -> r -> m a
runRWST_ s rwst r = (\(a,_,_) -> a) `liftM` runRWST rwst r s

browseDGS :: DGS a -> IO a
browseDGS = withSocketsDo . withManager . runRWST_ def . runDGS
