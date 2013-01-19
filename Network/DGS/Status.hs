{-# LANGUAGE FlexibleInstances, GADTs, ScopedTypeVariables, OverloadedStrings #-}
module Network.DGS.Status
	( Warning
	, Status(..)
	, Order(..)
	, quickStatus
	) where

-- TODO: clean up imports and exports
import Control.Applicative
import Data.Attoparsec as A
import Data.ByteString as B
import Data.ByteString.Lazy as B (toChunks)
import Data.Int
import Data.List
import Data.Monoid
import Network.DGS.Monad
import Network.DGS.Misc
import Network.DGS.Status.Bulletin
import Network.DGS.Status.Game
import Network.DGS.Status.Internal
import Network.DGS.Status.Message
import Network.DGS.Status.MultiplayerGame

-- TODO: look at and use the headers to make the parser more flexible

type Warning  = ByteString
data Status a = Status
	{ warnings         :: [Warning]
	, bulletins        :: [Bulletin]
	, messages         :: [Message]
	, games            :: [Game a]
	, multiplayerGames :: [MultiplayerGame]
	} deriving (Eq, Ord, Show)

instance Monoid (Status a) where
	mempty = Status mempty mempty mempty mempty mempty
	mappend (Status w b m g mpg) (Status w' b' m' g' mpg')
		= Status (mappend w w') (mappend b b') (mappend m m') (mappend g g') (mappend mpg mpg')

injectWarning         w = mempty { warnings         = [w] }
injectBulletin        b = mempty { bulletins        = [b] }
injectMessage         m = mempty { messages         = [m] }
injectGame            g = mempty { games            = [g] }
injectMultiplayerGame g = mempty { multiplayerGames = [g] }

instance Priority a => Atto (Status a) where
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

data Order a where
	StatusPage :: Order () -- TODO: if the status page sort order is set to 'Priority', does the server return priorities for 'StatusPage' ordering?
	Arbitrary  :: Order ()
	LastMoved  :: Order ()
	MoveCount  :: Order ()
	Priority   :: Order Int16
	TimeLeft   :: Order ()

instance Eq  (Order a) where a == b = compare a b == EQ
instance Ord (Order a) where
	compare StatusPage StatusPage = EQ
	compare Arbitrary  Arbitrary  = EQ
	compare LastMoved  LastMoved  = EQ
	compare MoveCount  MoveCount  = EQ
	compare Priority   Priority   = EQ
	compare TimeLeft   TimeLeft   = EQ
	compare StatusPage _          = LT
	compare _          StatusPage = GT
	compare Arbitrary  _          = LT
	compare _          Arbitrary  = GT
	compare LastMoved  _          = LT
	compare _          LastMoved  = GT
	compare MoveCount  _          = LT
	compare _          MoveCount  = GT
	compare Priority   _          = LT
	compare _          Priority   = GT

instance Show (Order a) where
	show StatusPage = "StatusPage"
	show Arbitrary  = "Arbitrary"
	show LastMoved  = "LastMoved"
	show MoveCount  = "MoveCount"
	show Priority   = "Priority"
	show TimeLeft   = "TimeLeft"

instance Read (Order ()) where
	readsPrec _ s =
		"StatusPage" --> StatusPage ++
		"Arbitrary"  --> Arbitrary  ++
		"LastMoved"  --> LastMoved  ++
		"MoveCount"  --> MoveCount  ++
		"TimeLeft"   --> TimeLeft
		where
		t --> v = case stripPrefix t s of
			Just rest -> [(v, rest)]
			_ -> []

instance Read (Order Int16) where
	readsPrec _ s = case stripPrefix "Priority" s of
		Just rest -> [(Priority, rest)]
		_ -> []

quickStatus :: forall a. Order a -> DGS (Status a)
quickStatus o = do
	server   <- asks fst
	response <- get id (uri server "quick_status.php") [("version","2"),("order",orderString)]
	let strictResponse = strictify response
	case (parseError strictResponse, parseOnly attoparser strictResponse) of
		(Just e,  _) -> throwError (DGSProblem e)
		(_, Left  _) -> throwError (NoParse response)
		(_, Right v) -> return v
	where
	attoparser :: Parser (Status a)
	(attoparser, orderString) = case o of
		StatusPage -> (attoparse, "")
		Arbitrary  -> (attoparse, "0")
		LastMoved  -> (attoparse, "1")
		MoveCount  -> (attoparse, "2")
		Priority   -> (attoparse, "3")
		TimeLeft   -> (attoparse, "4")
	strictify = B.concat . B.toChunks
