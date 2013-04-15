{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module Network.DGS.User where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Ratio
import Data.SGF.Types hiding (round)
import Data.Text
import Data.Text.Encoding
import Data.Text.Read
import Data.Time
import Network.DGS.Atto
import Network.DGS.Misc
import Network.DGS.Time

-- | Specifically for strings that happen to be users' nicknames.
newtype Nick = Nick { getNick :: Text } deriving (Eq, Ord, Show, Read)

-- nicks can only contain a-z, A-Z, 0-9, and -_+, which are pretty
-- much the same in all encodings, so just take a stab at one
instance Atto Nick where attoparse = Nick . decodeUtf8 <$> quotedField
instance FromJSON Nick where
	parseJSON (String t) = return (Nick t)
	parseJSON v = typeMismatch "string" v

newtype ELO = ELO { getELO :: Rational } deriving (Eq, Ord, Show, Read)

instance FromJSON ELO where
	parseJSON (String s) = case rational s of
		Right (r, s') | s' == "" -> pure (ELO r)
		_ -> typeMismatch "rating (as a floating-point number)" (String s)
	parseJSON v = typeMismatch "rating (as a floating-point number)" v

-- | for use with 'ID'
data UserTag

-- | The @miniUser@ parameter is typically 'MiniUser' or '()', and
-- the @ratingDiff@ parameter is typically 'Rational' or '()'.
data GamePlayer miniUser ratingDiff = GamePlayer
	{ this       :: ID UserTag
	, prisoners  :: Integer
	, remaining  :: Remaining
	, start      :: ELO       -- TODO: what does this do for users that have no rating?
	, end        :: Maybe ELO -- TODO: does this distinguish between finished games between unrated players and unfinished games between rated players?
	, miniUser   :: miniUser
	, ratingDiff :: ratingDiff
	} deriving (Eq, Ord, Show)

class    ParseDiff a        where parseDiff :: Value -> Parser a
instance ParseDiff ()       where parseDiff _ = pure ()
instance ParseDiff Rational where parseDiff = error "TODO: parsing for rating differences"

-- TODO: remove the crazy context and also the FlexibleContexts extension
instance (FromJSON Remaining, FromJSON a, ParseDiff b) => FromJSON (GamePlayer a b) where
	parseJSON o@(Object v) = GamePlayer
		<$> v .:  "id"
		<*> v .:  "prisoners"
		<*> v .:  "remtime"
		<*> v .:  "rating_start_elo"
		<*> v .:? "rating_end_elo"
		<*> parseJSON o
		<*> parseDiff o

-- | A few extra details about a user that can be retrieved at the same time
-- you retrieve a game, if you like.
data MiniUser = MiniUser
	{ nick       :: Nick
	, name       :: Text
	, country    :: Text
	, current    :: Maybe ELO
	, lastAccess :: UTCTime
	} deriving (Eq, Ord, Show, Read)

instance FromJSON MiniUser where
	parseJSON (Object o) = MiniUser
		<$> o .: "handle"
		<*> o .: "name"
		<*> o .: "country"
		<*> o .? "rating_elo"
		<*> (unDGSTime <$> o .: "last_access")
	parseJSON v = typeMismatch "user information" v

-- | Lossy conversion from high-precision ranks to low-precision ranks. Always
-- returns something that matches the patterns @Ranked _ Kyu Nothing@ or
-- @Ranked _ Dan Nothing@.
fromELO :: ELO -> Rank
fromELO (ELO r_) = Ranked n s Nothing where
	r = round (r_/100)
	s = if r < 21 then Kyu else Dan
	n = if r < 21 then 21 - r else r - 20

-- | Lossy conversion from ranks to ELO ratings. Rank certainty is ignored, and
-- 'Dan' and 'Pro' ranks are translated identically. Unknown ranks are assigned
-- the worst ELO rating, -900.
fromRank :: Rank -> ELO
fromRank (OtherRank {})   = ELO (-900)
fromRank (Ranked n Kyu _) = ELO (100*(21 - n) % 1)
fromRank (Ranked n _   _) = ELO (100*(20 + n) % 1)
