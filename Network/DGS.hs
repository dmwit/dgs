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

    -- * Listing games and messages
    status,
    statusUID,
    statusUser,

    -- * Making a move
    move,
    sgf,

    -- * Miscellaneous
    module Network.Browser,
    development,
    production,
    silence,
    browseDGS
) where

import Data.List
import Data.List.Split
import Network.Browser
import Network.DGS.Types
import Network.HTTP
import Network.URI
import qualified Network.Browser   as B
import qualified Network.DGS.Types as T
-- }}}
-- helpers {{{
uri :: String -> String -> URI
uri server path = full where
    auth = URIAuth { uriRegName = server, uriUserInfo = "", uriPort = "" }
    full = nullURI { uriScheme = "http:", uriAuthority = Just auth, uriPath = '/' : path }

form :: RequestMethod -> (String -> a) -> URI -> [(String, String)] -> DGS a
get  ::                  (String -> a) -> URI -> [(String, String)] -> DGS a
post ::                  (String -> a) -> URI -> [(String, String)] -> DGS a
form t f uri = T.DGS . fmap (f . rspBody . snd) . request . formToRequest . Form t uri
get          = form GET
post         = form POST

-- | by default, HTTP's browser chatters a lot on stdout; this action turns off
-- the chatter
silence :: DGS ()
silence = T.DGS $ setErrHandler quiet >> setOutHandler quiet where quiet _ = return ()

browseDGS :: DGS a -> IO a
browseDGS = B.browse . T.runDGS
-- }}}
-- servers {{{
-- | the address of the development server, @\"dragongoserver.sourceforge.net\"@
development :: String
-- | the address of the most well-known public server, @\"www.dragongoserver.net\"@
production  :: String
development = "dragongoserver.sourceforge.net"
production  = "www.dragongoserver.net"
-- }}}
-- via documented API {{{
-- login {{{
-- | Almost all commands require you to be logged in. Logging in does not count
-- against your quota; one side effect is that this will report success even if
-- you have already exceeded your quota.
login :: String -- ^ server, e.g. 'development' or 'production'
      -> String -- ^ user name
      -> String -- ^ password
      -> DGS LoginResult
login server username password = get resultFromString loc opts where
    loc  = uri server "login.php"
    opts = [("quick_mode", "1"), ("userid", username), ("passwd", password)]

    resultFromString s
        | "#Error: wrong_userid"   `isInfixOf` s = T.WrongUsername
        | "#Error: wrong_password" `isInfixOf` s = T.WrongPassword
        | "\nOk"                       ==      s = T.LoginSuccess
        | otherwise                              = T.LoginProblem s
-- }}}
-- status {{{
-- | get the inbox and games list of whoever is currently logged in
status      :: String -- ^ server
            -> DGS String
-- | get the games list of an arbitrary user
statusUID   :: String  -- ^ server
            -> Integer -- ^ user ID
            -> DGS String
-- | get the games list of an arbitrary user
statusUser  :: String  -- ^ server
            -> String  -- ^ user name
            -> DGS String

status     server      = get id (uri server "quick_status.php") []
statusUID  server uid  = get id (uri server "quick_status.php") [("uid", show uid)]
statusUser server user = get id (uri server "quick_status.php") [("user", user)]
-- }}}
-- play {{{
move :: String  -- ^ server
     -> Integer -- ^ game ID
     -> Bool    -- ^ playing as black? (can use exactly the value you got from 'status')
     -> Point   -- ^ the move the opponent just made
     -> Point   -- ^ your move
     -> DGS String
move server gid black old new = get id loc opts where
    loc  = uri server "quick_play.php"
    opts = [("gid", show gid), ("color", col), ("sgf_prev", point old), ("sgf_move", point new)]
    col  = if black then "B" else "W"
    point (x, y) = [pos x, pos y]
    pos   i      = toEnum (fromEnum 'a' + fromEnum i)
-- }}}
-- sgf {{{
-- | you can only get private comments if you are logged in; if you are not
-- logged in, this will succeed, but a request for private comments will be
-- ignored, and you'll get an SGF with only the public comments
sgf :: String  -- ^ server
    -> Integer -- ^ game ID
    -> Bool    -- ^ request the private comments?
    -> DGS String
sgf server gid comments = get id (uri server "sgf.php") opts where
    opts = [("gid", show gid), ("owned_comments", show . fromEnum $ comments)]
-- }}}
-- }}}
-- via (undocumented) screen scraping {{{
-- game notes {{{
-- | set whether game notes are visible for a particular game; if you are not
-- logged in, nothing happens
noteVisibility :: String  -- ^ server
               -> Integer -- ^ game ID
               -> Bool    -- ^ set them to be visible?
               -> DGS ()
noteVisibility server gid visible = post (const ()) (uri server "game.php") opts where
    opts = [("gid", show gid), ("hidenotes", if visible then "N" else "Y"), ("togglenotes", "Hide notes")]
-- }}}
-- }}}
