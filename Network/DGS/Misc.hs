module Network.DGS.Misc where

import Network.DGS.Monad
import Data.ByteString.Lazy.Char8 as L

-- | the address of the development server, @\"dragongoserver.sourceforge.net\"@
development :: String
-- | the address of the most well-known public server, @\"www.dragongoserver.net\"@
production  :: String
development = "dragongoserver.sourceforge.net"
production  = "www.dragongoserver.net"

data LoginResult
	= WrongUsername
	| WrongPassword
	| LoginProblem ByteString -- ^ it's a bug in the library if one of these ever gets built
	| LoginSuccess
	deriving (Eq, Ord, Show, Read)

-- TODO: see http://www.dragongoserver.net/forum/read.php?forum=10&thread=33711

-- | Almost all commands require you to be logged in. Logging in does not count
-- against your quota; one side effect is that this will report success even if
-- you have already exceeded your quota.
login :: String -- ^ server, e.g. 'development' or 'production'
      -> String -- ^ user name
      -> String -- ^ password
      -> DGS LoginResult
login server username password = get result loc opts where
	loc  = uri server "login.php"
	opts = [("quick_mode", "1"), ("userid", username), ("passwd", password)]

	result bs
		| L.pack "[#Error: wrong_userid;"   `L.isPrefixOf` bs = WrongUsername
		| L.pack "[#Error: wrong_password;" `L.isPrefixOf` bs = WrongPassword
		| L.pack "\nOk" == bs = LoginSuccess
		| otherwise           = LoginProblem bs

-- | Phantom type to prevent the mixing of different kinds of IDs -- game IDs,
-- tournament IDs, move IDs, user IDs, etc.
newtype ID a = ID { getID :: Integer } deriving (Eq, Ord, Show, Read)
