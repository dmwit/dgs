{-# LANGUAGE FlexibleInstances, LambdaCase #-}
module Network.DGS.Status.Game where

import Control.Applicative
import Data.Attoparsec
import Data.ByteString.Char8
import Data.Int
import Data.Ix
import Data.SGF.Types (Color(..))
import Data.Time
import Network.DGS.Game hiding (Game(..))
import Network.DGS.Misc
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
	-- TODO: delete after checking the real server's output
	-- , handicap           :: Integer
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
	attoparse = quoted (go <|> teamGo <|> zenGo) where
		quoted p = do
			word8 (enum '\'')
			v <- p
			word8 (enum '\'')
			return v

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

parseGameWith :: (Int16 -> a) -> Parser (Game a)
parseGameWith f = do
	word8 (enum 'G')
	gid_                <- comma >> attoparse
	opponent_           <- comma >> attoparse
	nextToMove_         <- comma >> attoparse
	lastMove_           <- comma >> attoparse
	timeRemaining_      <- comma >> attoparse
	action_             <- comma >> attoparse
	status_             <- comma >> attoparse
	mid_                <- comma >> attoparse
	tid_                <- comma >> attoparse
	sid_                <- comma >> attoparse
	style_              <- comma >> attoparse
	priority_           <- comma >> attoparse
	opponentLastAccess_ <- comma >> attoparse
	-- TODO: delete after checking the real server's output
	-- handicap_           <- comma >> attoparse
	return Game
		{ gid                = gid_
		, opponent           = opponent_
		, nextToMove         = nextToMove_
		, lastMove           = lastMove_
		, timeRemaining      = timeRemaining_
		, action             = action_
		, status             = status_
		, mid                = mid_
		, tid                = tid_
		, sid                = sid_
		, style              = style_
		, priority           = f priority_
		, opponentLastAccess = opponentLastAccess_
		-- TODO: delete after checking the real server's output
		-- , handicap           = handicap_
		}

instance Atto (Game Int16) where attoparse = parseGameWith id
instance Atto (Game ()   ) where attoparse = parseGameWith (const ())
