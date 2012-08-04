{-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies #-}
module Network.DGS.Types.Base where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.RWS
import Control.Monad.Trans.Control
import Data.ByteString.Lazy
import Data.Conduit
import Data.Text (Text)
import Data.Time
import Network.DGS.Types.Errors
import Network.HTTP.Conduit

-- | a monad stack to ease the use of http-conduit
newtype DGS a = DGS { runDGS :: DGS' a } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadUnsafeIO, MonadResource, MonadReader Manager, MonadState CookieJar, MonadBase IO)
type DGS' = RWST Manager () CookieJar (ResourceT IO)

-- this code is really unreadable, but it was written by just following the
-- types and applying the DGS/runDGS and StM/unStM isomorphisms as necessary
instance MonadBaseControl IO DGS where
	data StM DGS a = StM !(StM DGS' a)
	liftBaseWith f = DGS (liftBaseWith (\g -> f (\(DGS m) -> StM <$> g m)))
	restoreM (StM v) = DGS (restoreM v)

data LoginResult
	= WrongUsername
	| WrongPassword
	| LoginProblem ByteString -- ^ it's a bug in the library if one of these ever gets built
	| LoginSuccess
	deriving (Eq, Ord, Show, Read)

data Response a
	= Success        Quota a      -- ^ the stars aligned; here's your answer!
	| UnknownVersion Quota String -- ^ currently, only 1.0.15:13 is supported
	| Problem        Quota Error  -- ^ something was wrong with your request
	| NoParse                     -- ^ the server sent invalid JSON or valid JSON outside the schema it promised to deliver
	deriving (Eq, Ord, Show, Read)

-- | how many accesses you have left, and when the quota will reset to its
-- maximum
data Quota = Quota Integer UTCTime deriving (Eq, Ord, Show, Read)

-- | 0-indexed x/y coordinates that start at the top left
type Point = (Integer, Integer)

-- | Phantom type to prevent the mixing of different kinds of IDs -- game IDs,
-- tournament IDs, move IDs, user IDs, etc.
newtype ID a = ID { getID :: Integer } deriving (Eq, Ord, Show, Read)

-- | Specifically for strings that happen to be users' nicknames.
newtype Username = Username { getUsername :: Text } deriving (Eq, Ord, Show, Read)

-- | for use with 'ID'
data UserTag

-- | for use with 'ID'
data GameTag

-- | for use with 'ID'
data TournamentTag
