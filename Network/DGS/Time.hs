{-# LANGUAGE OverloadedStrings #-}
module Network.DGS.Time where

import Data.Time.Clock
import Network.DGS.Atto

-- spec for this stuff is at
-- http://dragongoserver.cvs.sourceforge.net/viewvc/dragongoserver/DragonGoServer/specs/time.txt?view=markup

-- | Dragon supports three standard kinds of byoyomi time.
data ByoyomiStyle
	= Japanese { periods :: Integer } -- ^ You have this many chances to take longer than the byoyomi time to play a move. If you use up all your chances, you lose.
	| Canadian { stones  :: Integer } -- ^ You must play this many moves in each byoyomi time. If you don't play enough moves in a byoyomi time, you lose.
	| Fischer                         -- ^ Every move increases your main time by the byoyomi time (up to a maximum of the game's original main time limit). If your main time runs out, you lose.
	deriving (Eq, Ord, Show, Read)

-- The style looks a bit weird below: the haddocks for Absolute and Main have
-- seemingly spurious blank lines after them and then some | style comments
-- that would look better as ^ style comments.  This is to work around some
-- deficiencies in Haddock, so please don't "fix" it.

-- | The time limit of a game: how much time the players are apportioned at the
-- beginning of the game.
data Limit
	= Absolute { main :: DiffTime } -- ^ 'Absolute' time limits have no extra time; once the 'main' time is up, that's it.

	-- | 'Indefinite' time limits are flexible: there's no upper bound on the
	-- actual amount of time a game may take, so long as both players play
	-- frequently once the 'main' time is up.
	| Indefinite
		{ main    :: DiffTime
		, byoyomi :: DiffTime -- ^ how much extra time is allotted; the interpretation of this field depends on the 'ByoyomiStyle' stored in 'style'
		, style   :: ByoyomiStyle
		}
	deriving (Eq, Ord, Show)

-- | One player's remaining time: how much time that player has left of the
-- amount originally apportioned to him at the beginning of the game. When an
-- opponent adds time to a player currently in a byoyomi period, that period
-- (and possibly all periods, at the opponent's disgression) is reset and the
-- player enters main time.
data Remaining
	= Main Limit -- ^ the player still has main time left

	-- | the player has used up all their main time and is in a (Japanese-style) byoyomi period
	| JapaneseByoyomi
		{ current       :: DiffTime -- ^ the amount of time remaining in the current period
		, extra         :: DiffTime -- ^ the amount of time available in a full period
		, extraPeriods  :: Integer  -- ^ how many full periods remain
		}

	-- | the player has used up all their main time and is in a (Canadian-style) byoyomi period
	| CanadianByoyomi
		{ current       :: DiffTime -- ^ the amount of time remaining in the current period
		, currentStones :: Integer  -- ^ how many stones must be played to finish this period
		, extra         :: DiffTime -- ^ the amount of time available in a full period
		, extraStones   :: Integer  -- ^ how many stones must be played to finish a full period
		}
	deriving (Eq, Ord, Show)

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
