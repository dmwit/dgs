{-# LANGUAGE LambdaCase #-}
module Network.DGS.Status.Game where

import Control.Applicative
import Data.Attoparsec
import Data.ByteString.Char8
import Data.Int
import Data.Ix
import Data.SGF.Types (Color(..))
import Data.Time
import Network.DGS.Game
import Network.DGS.Misc
import Network.DGS.Status.Internal
import Network.DGS.Time
import Network.DGS.User

-- | The @priority@ type variable will be either 'Int16' or '()', depending on
-- whether you ask for the games to be priority-ordered or not.
data Game priority = Game
	{ gid                :: ID GameTag
	, opponent           :: ID UserTag
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
	attoparse = natural >>= \case
		0  -> return Unsupported
		1  -> return PlaceHandicap
		2  -> return Move
		3  -> return Score
		10 -> return Bid
		11 -> return BidOrAccept
		12 -> return ChooseColor
		13 -> return Wait
		_  -> fail "expecting one of the known action codes: 0-3 or 10-13"

instance Atto Status where
	attoparse = choice
		[ "KOMI"   --> Komi
		, "PLAY"   --> Play
		, "PASS"   --> Pass
		, "SCORE"  --> Scoring
		, "SCORE2" --> Scoring2
		]

instance Atto Style where
	attoparse = go <|> teamGo <|> zenGo where
		go     = string (pack "GO") >> return Plain
		teamGo = do
			string (pack "TEAM_GO(")
			n <- natural
			word8 (enum ':')
			m <- natural
			word8 (enum ')')
			return (Team n m)
		zenGo  = do
			string (pack "ZEN_GO(")
			n <- natural
			word8 (enum ')')
			return (Zen n)

instance Atto Int16 where
	attoparse = natural >>= \n -> if inRange (-32768,32767) n then return (fromIntegral n) else fail $ "number out of range for an Int16: " ++ show n
