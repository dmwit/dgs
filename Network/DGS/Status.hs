{-# LANGUAGE OverloadedStrings #-}
module Network.DGS.Status
	( Warning
	, Status(..)
	, Order(..)
	, quickStatus
	) where

import Data.Attoparsec as A
import Data.ByteString as B
import Data.ByteString.Lazy as B (toChunks)
import Data.List
import Data.Monoid
import Network.DGS.Monad
import Network.DGS.Status.Bulletin
import Network.DGS.Status.Game
import Network.DGS.Status.Internal
import Network.DGS.Status.Message
import Network.DGS.Status.MultiplayerGame

-- TODO: look at and use the headers to make the parser more flexible

type Warning = ByteString
data Status  = Status
	{ warnings         :: [Warning]
	, bulletins        :: [Bulletin]
	, messages         :: [Message]
	, games            :: [Game]
	, multiplayerGames :: [MultiplayerGame]
	} deriving (Eq, Ord, Show)

instance Monoid Status where
	mempty = Status mempty mempty mempty mempty mempty
	mappend (Status w b m g mpg) (Status w' b' m' g' mpg')
		= Status (mappend w w') (mappend b b') (mappend m m') (mappend g g') (mappend mpg mpg')

injectWarning         w = mempty { warnings         = [w] }
injectBulletin        b = mempty { bulletins        = [b] }
injectMessage         m = mempty { messages         = [m] }
injectGame            g = mempty { games            = [g] }
injectMultiplayerGame g = mempty { multiplayerGames = [g] }

instance Atto Status where
	-- blank must come at the end and multiplayerGame must come before message
	attoparse = mconcat <$> many (choice [warning, multiplayerGame, bulletin, message, game, comment, blank] <* string "\n") where
		warning         = string "[#" >> injectWarning <$> restOfLine
		bulletin        = injectBulletin        <$> attoparse
		message         = injectMessage         <$> attoparse
		game            = injectGame            <$> attoparse
		multiplayerGame = injectMultiplayerGame <$> attoparse
		comment         = string "#" >> restOfLine >> return mempty
		blank           = return mempty
		restOfLine      = A.takeWhile (/= enum '\n')

data Order = StatusPage | Arbitrary | LastMoved | MoveCount | Priority | TimeLeft
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | The DGS server has a slight bug: it does not take the requested ordering
-- into account when doing its caching. This means you may get data in the
-- wrong order (and with wrong 'priority' fields) if you call 'quickStatus'
-- with two different 'Order' parameters within the cache-invalidation period
-- (usually a minute or so).
quickStatus :: Order -> DGS Status
quickStatus o = do
	server   <- asks fst
	response <- get id (uri server "quick_status.php") [("version","2"),("order",orderString)]
	let strictResponse = strictify response
	case (parseError strictResponse, parseOnly attoparse strictResponse) of
		(Just e,  _) -> throwError (DGSProblem e)
		(_, Left  _) -> throwError (NoParse response)
		(_, Right v) -> return v
	where
	orderString = case o of
		StatusPage -> ""
		Arbitrary  -> "0"
		LastMoved  -> "1"
		MoveCount  -> "2"
		Priority   -> "3"
		TimeLeft   -> "4"
	strictify = B.concat . B.toChunks
