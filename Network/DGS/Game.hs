module Network.DGS.Game where

import Control.Applicative
import Data.Aeson
import Data.Int
import Data.SGF.Types
import Data.Text
import Data.Time
import Network.DGS.Misc
import Network.DGS.Time
import Network.DGS.User

-- | for use with 'ID'
data GameTag

-- | for use with 'ID'
data TournamentTag

-- | for use with 'ID'
data ShapeTag

-- | for use with 'ID'
data MoveTag

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
	| Scoring
	| Scoring2
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

-- | The parameters will be '()' or something interesting; the something
-- interesting is 'MiniUser' for @miniUser@ and 'Rational' for @ratingDiff@.
data Game miniUser ratingDiff = Game
	{ this               :: ID GameTag
	, double             :: DoubleGame
	, tournament         :: Maybe (ID TournamentTag)
	, action             :: Action
	, status             :: Status
	, hasHiddenMessages  :: Bool
	, endedByAdmin       :: Bool
	, score              :: Maybe GameResult -- ^ 'Nothing' for unfinished games
	, style              :: Style
	, rated              :: Bool
	, ruleset            :: RuleSetGo        -- ^ 'Chinese' or 'Japanese'
	, size               :: Integer
	, komi               :: Rational
	, jigoMode           :: JigoMode
	, handicap           :: Integer
	, freePlacement      :: Bool
	, shape              :: ID ShapeTag
	, start              :: UTCTime
	, lastMove           :: UTCTime
	, timeRunsOnWeekends :: Bool
	, timeLimit          :: Limit
	, protagonist        :: ID UserTag
	, antagonist         :: ID UserTag
	, move               :: (ID MoveTag, Maybe Point)
	, nextPlayer         :: (Color, ID UserTag)
	, priority           :: Int16            -- ^ this will be zero unless you ask for the games to be sorted by priority and specify that you want priorities to be loaded
	, notes              :: Maybe Text
	, black              :: GamePlayer miniUser ratingDiff
	, white              :: GamePlayer miniUser ratingDiff
	} deriving (Eq, Ord, Show)
