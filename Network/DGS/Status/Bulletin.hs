module Network.DGS.Status.Bulletin where

import Data.Text
import Data.Time
import Network.DGS.Bulletin
import Network.DGS.Misc

data TargetType
	= All
	| TournamentDirector
	| TournamentParticipant
	| List
	| MultiplayerGame
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Category
	= Maintenance
	| Administrative
	| NewTournament
	| TournamentNews
	| Feature
	| Private
	| Advertisement
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Bulletin = Bulletin
	{ id          :: ID BulletinTag
	, targetType  :: TargetType
	, category    :: Category
	, publishTime :: UTCTime
	, expireTime  :: UTCTime
	, subject     :: Text
	} deriving (Eq, Ord, Show, Read)
