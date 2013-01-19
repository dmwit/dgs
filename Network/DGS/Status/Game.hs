{-# LANGUAGE LambdaCase #-}
module Network.DGS.Status.Game (Game(..), Priority(..), Int16) where

import Data.Int
import Data.Ix
import Data.SGF.Types (Color(..))
import Network.DGS.Game hiding (Game(..))
import Network.DGS.Status.Internal
import Network.DGS.Time hiding (style)
import Network.DGS.User

-- | The @priority@ type variable will be either 'Int16' or '()', depending on
-- whether you ask for the games to be priority-ordered or not.
data Game priority = Game
	{ gid                :: ID GameTag
	, opponent           :: Nick
	, nextToMove         :: Color
	, lastMove           :: UTCTime
	, timeRemaining      :: Limit
	, action             :: Action
	, status             :: Status
	, mid                :: ID MoveTag
	, tid                :: ID TournamentTag
	, sid                :: ID ShapeTag
	, style              :: Style
	, priority           :: priority
	, opponentLastAccess :: UTCTime
	, handicap           :: Integer
	} deriving (Eq, Ord, Show)

instance Atto Color where
	attoparse = choice ["B" --> Black, "W" --> White]

instance Atto Limit where
	attoparse = quotedField >> return (Absolute 0) -- TODO

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

instance Atto Status where
	attoparse = choice
		[ "KOMI"   --> Komi
		, "PLAY"   --> Play
		, "PASS"   --> Pass
		-- SCORE2 must come before SCORE so that SCORE doesn't succeed and commit!
		, "SCORE2" --> Scoring2
		, "SCORE"  --> Scoring
		]

instance Atto Int16 where
	attoparse = attoparse >>= \n -> if inRange (-32768,32767) n then return (fromInteger n) else fail $ "number out of range for an Int16: " ++ show n

quoted p = do
	word8 (enum '\'')
	v <- p
	word8 (enum '\'')
	return v

class    Priority a     where toPriority :: Int16 -> a
instance Priority Int16 where toPriority = id
instance Priority ()    where toPriority = const ()

instance Priority a => Atto (Game a) where
	attoparse = "G" --> Game
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
		<*> (comma >> quoted attoparse)
		<*> (toPriority <$> column)
		<*> column
		<*> column
