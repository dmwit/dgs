-- boilerplate {{{
{-|
TODO: update this introduction

This is an interface to Dragon Go Server's robot interface, as outlined at
<http://dragongoserver.cvs.sourceforge.net/viewvc/dragongoserver/DragonGoServer/specs/quick_suite.txt>.

Here are some sample interactions from ghci, with a fictitious password:

> *Network.DGS> browseDGS (silence >> login development "smartypants" "password")
> LoginSuccess
> *Network.DGS> browseDGS (silence >> statusUID production 4155) >>= mapM_ print
> (453881,"jedge42",False,"2009-12-21 03:14 GMT","F: 30d 1h")
> (532927,"bartnix",False,"2009-12-20 06:06 GMT","F: 21d 13h")
> *Network.DGS> browseDGS (silence >> statusUser production "dmwit") >>= mapM_ print
> (453881,"jedge42",False,"2009-12-21 03:14 GMT","F: 30d 1h")
> (532927,"bartnix",False,"2009-12-20 06:06 GMT","F: 21d 13h")
> *Network.DGS> :{
> *Network.DGS| browseDGS $ do {
> *Network.DGS|   silence;
> *Network.DGS|   login development "smartypants" "password";
> *Network.DGS|   (_, (gid, _, black, _, _):_) <- status development;
> *Network.DGS|   move development gid black (16, 18) (17, 16)
> *Network.DGS|   }
> *Network.DGS| :}
> MoveSuccess
-}

module Network.DGS (
    -- * Logging in
    login,

    -- * Making a move
    sgf,

    -- * Miscellaneous
    development,
    production,
    browseDGS
) where

import Control.Monad.RWS hiding (get)
import Control.Monad.Trans
import Data.Time
import Network.DGS.Types
import Network.DGS.JSON
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.Socket
import qualified Network.DGS.Types as T
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
-- }}}
-- helpers {{{
-- only call this with ASCII host and path parameters, please
uri :: String -> String -> Request m
uri host_ path_ = def { host = S.pack host_, path = S.pack ('/':path_) }

form :: Method -> (L.ByteString -> a) -> Request DGS -> [(String, String)] -> DGS a
get  ::           (L.ByteString -> a) -> Request DGS -> [(String, String)] -> DGS a
post ::           (L.ByteString -> a) -> Request DGS -> [(String, String)] -> DGS a
form t f r_ q = do
	manager <- ask
	now     <- liftIO getCurrentTime
	r       <- state (\cookies -> insertCookiesIntoRequest r_ { queryString = renderQuery False (toQuery q) } cookies now)
	resp_   <- httpLbs r manager
	now     <- liftIO getCurrentTime
	resp    <- state ((\(a,b) -> (b,a)) . updateCookieJar resp_ r now)
	return . f . responseBody $ resp

get  = form methodGet
post = form methodPost
object obj cmd opts server = get
	(maybe NoParse id . decode)
	(uri server "quick_do.php")
	(("obj",obj):("cmd",cmd):opts)

runRWST_ :: Monad m => s -> RWST r w s m a -> r -> m a
runRWST_ s rwst r = (\(a,_,_) -> a) `liftM` runRWST rwst r s

browseDGS :: DGS a -> IO a
browseDGS = withSocketsDo . withManager . runRWST_ def . runDGS
-- }}}
-- servers {{{
-- | the address of the development server, @\"dragongoserver.sourceforge.net\"@
development :: String
-- | the address of the most well-known public server, @\"www.dragongoserver.net\"@
production  :: String
development = "dragongoserver.sourceforge.net"
production  = "www.dragongoserver.net"
-- }}}
-- login {{{
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
		| L.pack "[#Error: wrong_userid;"   `L.isPrefixOf` bs = T.WrongUsername
		| L.pack "[#Error: wrong_password;" `L.isPrefixOf` bs = T.WrongPassword
		| L.pack "\nOk" == bs = T.LoginSuccess
		| otherwise           = T.LoginProblem bs
-- }}}
-- games {{{
gameInfo :: String -> ID GameTag -> DGS (T.Response (ID GameTag))
gameInfo server (ID gid) = object "game" "info" [("gid", show gid)] server
-- }}}
-- sgf {{{
-- TODO: update this to take advantage of all the new multi-player game stuff
-- from the new specs.
--
-- | you can only get private comments if you are logged in; if you are not
-- logged in, this will succeed, but a request for private comments will be
-- ignored, and you'll get an SGF with only the public comments
sgf :: String  -- ^ server
    -> Integer -- ^ game ID
    -> Bool    -- ^ request the private comments?
    -> DGS L.ByteString
sgf server gid comments = get id (uri server "sgf.php") opts where
    opts = [("gid", show gid), ("owned_comments", show . fromEnum $ comments)]
-- }}}
