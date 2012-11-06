module Network.DGS.Types.Time where

-- spec for this stuff is at
-- http://dragongoserver.cvs.sourceforge.net/viewvc/dragongoserver/DragonGoServer/specs/time.txt?view=markup

-- TODO:
-- * documentation
-- * check how things are reported when somebody adds time to somebody that's in a byoyomi period
-- * do something about the horribleness of hand-namespacing (e.g. AbsoluteLimit/AbsoluteRemaining)

data ByoyomiStyle
	= Japanese { periods :: Integer }
	| Canadian { stones  :: Integer }
	| Fischer
	deriving (Eq, Ord, Show, Read)

data Limit
	= AbsoluteLimit { main :: DiffTime }
	| ByoyomiLimit
		{ main    :: DiffTime
		, byoyomi :: DiffTime
		, style   :: ByoyomiStyle
		}
	deriving (Eq, Ord, Show, Read)

data Remaining
	= AbsoluteRemaining { current :: DiffTime }
	| MainRemaining
		{ current      :: DiffTime
		, extra        :: Difftime
		, extraStyle   :: ByoyomiStyle
		}
	| ByoyomiRemaining
		{ current      :: DiffTime
		, currentStyle :: ByoyomiStyle
		, extra        :: DiffTime
		, extraStyle   :: ByoyomiStyle
		}
	-- invariant: currentStyle and extraStyle have the same constructor
	deriving (Eq, Ord, Show, Read)

{- example of good documentation to steal copy from
data TimeRemaining = Byoyomi
	{ current :: DiffTime -- ^ the timer that's currently counting down (may be main time, or the time in a byoyomi)
	, extra   :: DiffTime -- ^ the time left in the next byoyomi period (usually the byoyomi time of the game's time limit, but may be less if the opponent added time in the middle of a byoyomi period); in Fischer time, this is how much will be added on the next move
	, stones  :: Integer  -- ^ the number of stones one must play to complete this period (if applicable)
	, periods :: Integer  -- ^ how many complete byoyomi periods have not yet been started
	} deriving (Eq, Ord, Show, Read)
-}
