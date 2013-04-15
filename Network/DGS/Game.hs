{-# LANGUAGE LambdaCase #-}
module Network.DGS.Game (module Network.DGS.Game, Int16) where

import Control.Applicative
import Data.Aeson
import Data.Int
import Data.SGF.Types (Color, GameResult, Point, RuleSetGo)
import Data.Text
import Data.Time
import Network.DGS.Atto
import Network.DGS.Misc
import Network.DGS.Time
import Network.DGS.User
import qualified Data.ByteString.Char8 as C

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

instance Atto Action where
	attoparse = attoparse >>= \case
		0  -> return Unsupported
		1  -> return PlaceHandicap
		2  -> return Move
		3  -> return Score
		10 -> return Bid
		11 -> return BidOrAccept
		12 -> return ChooseColor
		13 -> return Wait
		n  -> fail
		   $  "expecting one of the known action codes (0-3 or 10-13), but got "
		   ++ show (n :: Integer)
		   ++ " instead"

data Status
	= Komi
	| Setup
	| Play
	| Pass
	| Scoring
	| Scoring2
	| Finished
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance Atto Status where
	attoparse = choice
		[ "KOMI"   --> Komi
		, "PLAY"   --> Play
		, "PASS"   --> Pass
		-- SCORE2 must come before SCORE so that SCORE doesn't succeed and commit!
		, "SCORE2" --> Scoring2
		, "SCORE"  --> Scoring
		]

data Style
	= Plain
	| Zen Integer
	| Team Integer Integer
	deriving (Eq, Ord, Show, Read)

instance Atto Style where
	attoparse = go <|> teamGo <|> zenGo where
		go     = string (C.pack "GO") >> return Plain
		teamGo = do
			string (C.pack "TEAM_GO(")
			n <- natural
			word8 (enum ':')
			m <- natural
			word8 (enum ')')
			return (Team n m)
		zenGo  = do
			string (C.pack "ZEN_GO(")
			n <- natural
			word8 (enum ')')
			return (Zen n)

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
