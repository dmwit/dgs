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

-- documentation for how login.php behaves in quick mode is available at
-- http://www.dragongoserver.net/forum/read.php?forum=10&thread=33711

-- TODO: L.unpack below means we're assuming latin-1 encoding for error
-- reporting; is that really right?

-- | Almost all commands require you to be logged in. Logging in does not count
-- against your quota; one side effect is that this will succeed even if you
-- have already exceeded your quota.
login :: String -- ^ user name
      -> String -- ^ password
      -> DGS ()
login username password = do
	server <- asks fst
	action <- get result (uri server "login.php") opts
	action
	where
	opts = [("quick_mode", "1"), ("userid", username), ("passwd", password)]

	result bs = case L.unpack bs of
		"\nOk" -> return ()
		(parseError -> Just e) -> throwError (DGSProblem e)
		_ -> throwError (NoParse bs)

	parseError s = findError <$> do
		s      <- stripPrefix "[#Error: " s
		(e, d) <- case splitOn "; " s of
			(e:ds) -> return (e, intercalate "; " ds)
			_      -> Nothing
		return (UnknownError e d)

-- | Phantom type to prevent the mixing of different kinds of IDs -- game IDs,
-- tournament IDs, move IDs, user IDs, etc.
newtype ID a = ID { getID :: Integer } deriving (Eq, Ord, Show, Read)
