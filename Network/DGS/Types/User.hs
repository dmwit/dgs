module Network.DGS.Types.User where

import Data.Ratio
import Data.SGF.Types hiding (round)
import Data.Text
import Network.DGS.Types.Base

newtype ELO = ELO { getELO :: Rational } deriving (Eq, Ord, Show, Read)

-- | The parameter is typically 'Extra' or '()'.
data GamePlayer a = GamePlayer
	{ this      :: ID UserTag
	, prisoners :: Integer
	, remaining :: TimeRemaining
	, start     :: ELO       -- TODO: what does this do for users that have no rating?
	, end       :: Maybe ELO -- TODO: does this distinguish between finished games between unrated players and unfinished games between rated players?
	, extra     :: a
	} deriving (Eq, Ord, Show, Read)

-- | A few extra details about a user that can be retrieved at the same time
-- you retrieve a game, if you like.
data Extra = Extra
	{ nick      :: Nick
	, name      :: Text
	, current   :: ELO -- TODO: what does this do for users that have no rating?
	} deriving (Eq, Ord, Show, Read)

-- | Lossy conversion from high-precision ranks to low-precision ranks. Always
-- returns something that matches the patterns @Ranked _ Kyu Nothing@ or
-- @Ranked _ Dan Nothing@.
fromELO :: ELO -> Rank
fromELO (ELO r_) = Ranked n s Nothing where
	r = round (r_/100)
	s = if r < 21 then Kyu else Dan
	n = if r < 21 then 21 - r else r - 20

-- | Lossy conversion from ranks to ELO ratings. Rank certainty is ignored, and
-- 'Dan' and 'Pro' ranks are translated identically. Unknown ranks are assigned
-- the worst ELO rating, -900.
fromRank :: Rank -> ELO
fromRank (OtherRank {})   = ELO (-900)
fromRank (Ranked n Kyu _) = ELO (100*(21 - n) % 1)
fromRank (Ranked n _   _) = ELO (100*(20 + n) % 1)
