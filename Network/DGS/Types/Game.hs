module Network.DGS.Types.Game where

data DoubleGame
	= Single
	| Double        (ID GameTag)
	| DeletedDouble (ID GameTag)
	deriving (Eq, Ord, Show, Read)

-- | What action the player should take next in
-- a game. The 'Bid', 'BidOrAccept',
-- 'ChooseColor', and 'Wait' actions are for
-- fair-komi negotiation.
data Action
	= Unsupported -- ^ this probably shouldn't happen
	| PlaceHandicap
	| Move
	| Score
	| Bid
	| BidOrAccept
	| ChooseColor
	| Wait
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Status
	= Komi
	| Setup
	| Play
	| Pass
	| Score
	| Score2
	| Finished
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Style
	= Plain
	| Zen Integer
	| Team Integer Integer
	deriving (Eq, Ord, Show, Read)

data JigoMode
	= KeepKomi
	| AllowJigo
	| NoJigo
	deriving (Eq, Ord, Show, Read)

-- | The parameter will typically be 'Extra' or '()'.
data Game a = Game
	{ this               :: ID GameTag
	, double             :: DoubleGame
	, tournament         :: Maybe (ID TournamentTag)
	, action             :: Action
	, status             :: Status
	, hasHiddenMessages  :: Bool
	, endedByAdmin       :: Bool
	, score              :: Maybe GameResult -- ^ 'Nothing' for unfinished games
	, rated              :: Bool
	, style              :: Style
	, ruleset            :: RulesetGo        -- ^ 'Chinese' or 'Japanese'
	, size               :: Integer
	, komi               :: Rational
	, jigoMode           :: JigoMode
	, handicap           :: Integer
	, freePlacement      :: Bool
	, shape              :: ID ShapeTag
	, shapeSnapshot      :: Text             -- ^ don't really know what this is...
	, start              :: UTCTime
	, lastMove           :: UTCTime
	, timeRunsOnWeekends :: Bool
	, timeLimit          :: TimeLimit
	, move               :: (ID MoveTag, Maybe Point)
	, nextPlayer         :: (Color, ID UserTag)
	, ko                 :: Bool             -- ^ did the last move take a ko?
	, black              :: GamePlayer a
	, white              :: GamePlayer a
	} deriving (Eq, Ord, Show, Read)
