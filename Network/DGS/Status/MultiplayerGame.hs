module Network.DGS.Status.MultiplayerGame where

import Network.DGS.Status.Imports

data MultiplayerGame = MultiplayerGame
	{ gid         :: ID GameTag
	, style       :: Style
	, ruleset     :: RuleSetGo -- ^ 'Chinese' or 'Japanese'
	, size        :: Integer
	, lastChanged :: UTCTime
	, ready       :: Bool
	} deriving (Eq, Ord, Show, Read)

-- TODO: test this...?
instance Atto MultiplayerGame where
	attoparse = "MPG" --> MultiplayerGame
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
