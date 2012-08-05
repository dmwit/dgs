module Network.DGS.Types.Time where

-- TODO
data TimeLimit = TimeLimit deriving (Eq, Ord, Show, Read)

-- TODO double-check this is finished now, and make the documentation complete
data TimeRemaining = Byoyomi
	{ current :: DiffTime -- ^ the timer that's currently counting down (may be main time, or the time in a byoyomi)
	, extra   :: DiffTime -- ^ the time left in the next byoyomi period (usually the byoyomi time of the game's time limit, but may be less if the opponent added time in the middle of a byoyomi period); in Fischer time, this is how much will be added on the next move
	, stones  :: Integer  -- ^ the number of stones one must play to complete this period (if applicable)
	, periods :: Integer  -- ^ how many complete byoyomi periods have not yet been started
	} deriving (Eq, Ord, Show, Read)
