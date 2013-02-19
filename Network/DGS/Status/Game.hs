{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Network.DGS.Status.Game (Game(..), Int16) where

import Data.Int
import Data.Ix
import Data.Monoid
import Data.SGF.Types (Color(..))
import Network.DGS.Game hiding (Game(..))
import Network.DGS.Status.Internal
import Network.DGS.Time hiding (style)
import Network.DGS.User

data Game = Game
	{ gid                :: ID GameTag
	, opponent           :: Nick
	, nextToMove         :: Color
	, lastMove           :: UTCTime
	, timeRemaining      :: Remaining
	, action             :: Action
	, status             :: Status
	, mid                :: ID MoveTag
	, tid                :: ID TournamentTag
	, sid                :: ID ShapeTag
	, style              :: Style
	, priority           :: Int16 -- ^ this will be zero unless you ask for 'Priority' ordering or the user has set priority as their status page's sort order and you ask for 'StatusPage' ordering
	, opponentLastAccess :: UTCTime
	, handicap           :: Integer
	} deriving (Eq, Ord, Show)

instance Atto Color where
	attoparse = choice ["B" --> Black, "W" --> White]

{-
- m/B/b = "Xd Yh" (X days, Y hours), e.g. "2d 3h", "10d", "7h", "0h"
- p/P = [integer]

"J: m (-)"         : Japanese time, m main-time left (absolute time, no extra-periods)
"J: m (+ B * p)"   : Japanese time, m main-time left + extra-time (B time per move and p extra byo-yomi-periods left)
"J: b (B * p)"     : Japanese time, in byo-yomi (no '+'), b byo-yomi-time left + extra-time (B time per move and p extra byo-yomi-periods left)
"J: b (B * 0)"     : Japanese time, in byo-yomi (no '+'), b byo-yomi-time left, last byo-yomi-period

"C: m (-)"         : Canadian time, m main-time left (absolute time, no extra-time)
"C: m (+ B / P)"   : Canadian time, m main-time left + extra-time (B time per P stones)
"C: b / p (B / P)" : Canadian time, in byo-yomi (no '+'), b byo-yomi-time left for p stones to play (extra-time is B time per P stones)

"F: m (-)"         : Fischer time, m main-time left (absolute time, no extra-time)
"F: m (+ B)"       : Fischer time, m main-time left + extra-time (B time extra per move)
-}
instance Atto Remaining where
	attoparse = quoted (japanese <|> canadian <|> fischer) where
		japanese = tagged 'J' (parenthesized (jMain <|> jByo))
		canadian = tagged 'C' (parenthesized  cMain <|> cByo )
		fischer  = tagged 'F' (parenthesized  fMain          )

		jMain = do
			n <- string  "+ " >> hours
			p <- string " * " >> natural
			return (\m -> Main (Indefinite m n (Japanese p)))

		jByo = do
			n <-                 hours
			p <- string " * " >> natural
			return (\m -> JapaneseByoyomi m n p)

		cMain = do
			n <- string  "+ " >> hours
			p <- string " / " >> natural
			return (\m -> Main (Indefinite m n (Canadian p)))

		cByo = do
			pm      <- string "/ " >> natural
			(n, pn) <- string  " " >> parenthesized (liftA3 (\x y z -> (x,z)) hours (string " / ") natural)
			return (\m -> CanadianByoyomi m pm n pn)

		fMain = do
			n <- string "+ " >> hours
			return (\m -> Main (Indefinite m n Fischer))

		absolute = "(-)" --> (Main . Absolute)
		tagged c p = do
			(c:": ") --> ()
			v <- hours
			string " "
			f <- absolute <|> p
			return (f v)

hours = (*3600) <$> do
	n    <- natural
	unit <- string "d" <|> string "h"
	if unit == "d"
		then do
			h <- (string " " >> natural <* string "h") <|> return 0
			return (24*n+h)
		else return n

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

bracketed s e p = do
	word8 (enum s)
	v <- p
	word8 (enum e)
	return v

parenthesized = bracketed '(' ')'
quoted        = bracketed '\'' '\''

instance Atto Game where
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
		<*> column
		<*> column
		<*> column
