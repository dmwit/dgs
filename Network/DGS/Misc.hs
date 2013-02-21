{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Network.DGS.Misc where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.List.Split
import Data.Maybe
import Network.DGS.Monad
import Network.DGS.Errors
import qualified Data.ByteString.Lazy  as B (toChunks)
import qualified Data.ByteString.Char8 as B

-- | the address of the development server, @\"dragongoserver.sourceforge.net\"@
development :: Server
-- | the address of the most well-known public server, @\"www.dragongoserver.net\"@
production  :: Server
development = "dragongoserver.sourceforge.net"
production  = "www.dragongoserver.net"

-- documentation for how login.php behaves in quick mode is available at
-- http://www.dragongoserver.net/forum/read.php?forum=10&thread=33711

-- TODO: B.unpack below means we're assuming latin-1 encoding for error
-- reporting; is that really right?

-- | Almost all commands require you to be logged in. Logging in does not count
-- against your quota; one side effect is that this will succeed even if you
-- have already exceeded your quota.
login :: String -- ^ user name
      -> String -- ^ password
      -> DGS ()
login username password = get "login" opts >>= result where
	opts = [("quick_mode", "1"), ("userid", username), ("passwd", password)]

	result bs = case B.concat . B.toChunks $ bs of
		_ | bs == "\nOk"       -> return ()
		(parseError -> Just e) -> throwError (DGSProblem e)
		_                      -> throwError (NoParse bs)

parseError s = findError <$> do
	b <- stripPrefix "[#Error: " b_
	e <- stripPrefix "; "        e_
	return (UnknownError (B.unpack b) (B.unpack e))
	where
	stripPrefix pre s = guard (pre `B.isPrefixOf` s) >> return (B.drop (B.length pre) s)
	(b_, e_) = B.breakSubstring "; " s

-- | Phantom type to prevent the mixing of different kinds of IDs -- game IDs,
-- tournament IDs, move IDs, user IDs, etc.
newtype ID a = ID { getID :: Integer } deriving (Eq, Ord, Show, Read)

instance FromJSON (ID a) where
	parseJSON (Object v) = ID <$> v .: "id"
	parseJSON v = typeMismatch "object with ID number" v
