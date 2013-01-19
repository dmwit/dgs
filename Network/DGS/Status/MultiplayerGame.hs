module Network.DGS.Status.MultiplayerGame where

import Data.SGF.Types
import Network.DGS.Game hiding (Game(..))
import Network.DGS.Status.Internal

data MultiplayerGame = MultiplayerGame
	{ gid         :: ID GameTag
	, style       :: Style
	, ruleset     :: RuleSetGo -- ^ 'Chinese' or 'Japanese'
	, size        :: Integer
	, lastChanged :: UTCTime
	, ready       :: Bool
	} deriving (Eq, Ord, Show, Read)

instance Atto Bool where
	attoparse = "0" --> False <|> "1" --> True
instance Atto RuleSetGo where
	attoparse = "CHINESE" --> Chinese <|> "JAPANESE" --> Japanese

instance Atto MultiplayerGame where
	attoparse = "MPG" --> MultiplayerGame
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
