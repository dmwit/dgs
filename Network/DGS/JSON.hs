{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Network.DGS.JSON (decode) where

import Control.Applicative
import Data.Aeson hiding (Success)
import Data.Aeson.Types hiding (Success)
import Data.Text
import Data.Time.Format
import Network.DGS.Types
import System.Locale

import qualified Data.HashMap.Strict as H

instance FromJSON a => FromJSON (Response a) where
	parseJSON v@(Object o) = do
		quota   <- parseJSON v
		version <- o .: "version"
		error   <- o .: "error"
		case (version, error, label error) of
			("1.0.15:1", "", _) -> Success quota <$> parseJSON v
			("1.0.15:1", _ , l) -> Problem quota . maybe (UnknownError error) KnownError l <$> o .: "error_msg"
			_                   -> pure (UnknownVersion quota version)

instance FromJSON Quota where
	parseJSON (Object v) = case H.lookup "quota_expire" v of
		Just (String s) -> case parseTime defaultTimeLocale "%F %T" (unpack s) of
			Just t  -> Quota <$> v .: "quota_count" <*> pure t
			Nothing -> fail $ "quota expiration date in strange format: " ++ unpack s
		Just s  -> typeMismatch "date" s
		Nothing -> fail "quota expiration date missing"

instance FromJSON (ID GameTag) where
	parseJSON (Object v) = ID <$> v .: "id"
	parseJSON v = typeMismatch "game ID number" v
