module Network.DGS.Status.Bulletin where

import Network.DGS.Status.Imports hiding (count, All(..))

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
	{ bid         :: ID BulletinTag
	, targetType  :: TargetType
	, category    :: Category
	, publishTime :: UTCTime
	, expireTime  :: Maybe UTCTime  -- ^ the bulletin will automatically be marked as read at this time
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
		-- ADM_MSG must come before AD so that AD doesn't succeed and commit!
		, "ADM_MSG"  --> Administrative
		, "TOURNEY"  --> NewTournament
		, "TNEWS"    --> TournamentNews
		, "FEATURE"  --> Feature
		, "PRIV_MSG" --> Private
		, "AD"       --> Advertisement
		]

instance Atto Bulletin where
	attoparse = "B" --> Bulletin
		<*> column
		<*> column
		<*> column
		<*> column
		<*> (comma >> maybeDate)
		<*> (comma >> field)
		where maybeDate = (Just <$> attoparse) <|> ("''" --> Nothing)
