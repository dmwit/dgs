{-# LANGUAGE ViewPatterns #-}
module Network.DGS.Misc where

import Control.Applicative
import Data.List
import Data.List.Split
import Data.Maybe
import Network.DGS.Monad
import Network.DGS.Errors
import qualified Data.ByteString.Lazy.Char8 as L

-- | the address of the development server, @\"dragongoserver.sourceforge.net\"@
development :: Server
-- | the address of the most well-known public server, @\"www.dragongoserver.net\"@
production  :: Server
development = "dragongoserver.sourceforge.net"
production  = "www.dragongoserver.net"

data LoginResponse
	= LoginProblem Error
	| LoginNoParse String -- ^ it's a bug in the library if one of these ever gets built
	| LoginSuccess
	deriving (Eq, Ord, Show, Read)

-- documentation for how login.php behaves in quick mode is available at
-- http://www.dragongoserver.net/forum/read.php?forum=10&thread=33711

-- TODO: L.unpack below means we're assuming latin-1 encoding for error
-- reporting; is that really right?

-- | Almost all commands require you to be logged in. Logging in does not count
-- against your quota; one side effect is that this will report success even if
-- you have already exceeded your quota.
login :: String -- ^ server, e.g. 'development' or 'production'
      -> String -- ^ user name
      -> String -- ^ password
      -> DGS LoginResponse
login server username password = get (result . L.unpack) loc opts where
	loc  = uri server "login.php"
	opts = [("quick_mode", "1"), ("userid", username), ("passwd", password)]

	result "\nOk" = LoginSuccess
	result (parseError -> Just e) = LoginProblem e
	result s = LoginNoParse s

	parseError s = findError <$> do
		s      <- stripPrefix "[#Error: " s
		(e, d) <- case splitOn "; " s of
			(e:ds) -> return (e, intercalate "; " ds)
			_      -> Nothing
		return (UnknownError e d)

-- | Phantom type to prevent the mixing of different kinds of IDs -- game IDs,
-- tournament IDs, move IDs, user IDs, etc.
newtype ID a = ID { getID :: Integer } deriving (Eq, Ord, Show, Read)
