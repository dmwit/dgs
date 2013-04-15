module Network.DGS.Status.Game (Game(..), Int16) where

import Network.DGS.Status.Imports hiding (style)

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
