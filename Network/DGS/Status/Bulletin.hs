module Network.DGS.Status.Bulletin where

import Data.Attoparsec hiding (count)
import Data.ByteString hiding (count)
import Data.Time
import Network.DGS.Bulletin
import Network.DGS.Status.Internal
import Network.DGS.Misc

-- | who is this bulletin shown to?
data TargetType
	= All                   -- ^ everybody on DGS
	| TournamentDirector    -- ^ the director of a specific tournament
	| TournamentParticipant -- ^ participants in a specific tournament
	| List                  -- ^ a specific list of people
	| MultiplayerGame       -- ^ participants of a specific multiplayer game
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Category
	= Maintenance
	| Administrative
	| NewTournament
	| TournamentNews -- ^ announcement associated with a specific tournament
	| Feature        -- ^ information about a new feature
	| Private        -- ^ specific to you
	| Advertisement
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Bulletin = Bulletin
	{ count       :: Integer        -- TODO: what is this?
	, bid         :: ID BulletinTag
	, targetType  :: TargetType
	, category    :: Category
	, publishTime :: UTCTime
	, expireTime  :: UTCTime        -- ^ the bulletin will automatically be marked as read at this time
	, subject     :: ByteString     -- ^ no encoding defined for now, but probably latin-1, utf-8, or utf-16
	} deriving (Eq, Ord, Show, Read)

instance Atto TargetType where
	attoparse = choice
		[ "ALL" --> All
		, "TD"  --> TournamentDirector
		, "TP"  --> TournamentParticipant
		, "UL"  --> List
		, "MPG" --> MultiplayerGame
		]

instance Atto Category where
	attoparse = choice
		[ "MAINT"    --> Maintenance
		, "ADM_MSG"  --> Administrative
		, "TOURNEY"  --> NewTournament
		, "TNEWS"    --> TournamentNews
		, "FEATURE"  --> Feature
		, "PRIV_MSG" --> Private
		, "AD"       --> Advertisement
		]

instance Atto Bulletin where
	attoparse = do
		word8 (enum 'B')
		count_       <- comma >> attoparse
		bid_         <- comma >> attoparse
		targetType_  <- comma >> attoparse
		category_    <- comma >> attoparse
		publishTime_ <- comma >> attoparse
		expireTime_  <- comma >> attoparse
		subject_     <- comma >> field
		return Bulletin
			{ count       = count_
			, bid         = bid_
			, targetType  = targetType_
			, category    = category_
			, publishTime = publishTime_
			, expireTime  = expireTime_
			, subject     = subject_
			}
