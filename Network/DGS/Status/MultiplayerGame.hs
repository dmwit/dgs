module Network.DGS.Status.MultiplayerGame where

import Control.Applicative
import Data.Attoparsec
import Data.SGF.Types
import Data.Time
import Network.DGS.Game hiding (Game(..))
import Network.DGS.Misc
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
	-- TODO: yep, definitely use this trick instead of "tag" in the other instances
	attoparse = "MPG" --> MultiplayerGame
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
		<*> column
