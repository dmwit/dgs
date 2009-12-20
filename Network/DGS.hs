-- boilerplate {{{
module Network.DGS where

import Data.List
import Data.List.Split
import Network.Browser
import Network.HTTP
import Network.URI
-- }}}
-- helpers {{{
type DGS a = BrowserAction (HandleStream String) a

uri :: String -> String -> URI
uri server path = full where
    auth = URIAuth { uriRegName = server, uriUserInfo = "", uriPort = "" }
    full = nullURI { uriScheme = "http:", uriAuthority = Just auth, uriPath = '/' : path }

post :: URI -> [(String, String)] -> DGS (Response String)
post uri = fmap snd . request . formToRequest . Form POST uri

get :: URI -> DGS (Response String)
get = fmap snd . request . defaultGETRequest

valueOf :: (String -> a) -> DGS (Response String) -> DGS a
valueOf f = fmap (f . rspBody)

silence :: DGS ()
silence = setErrHandler quiet >> setOutHandler quiet where quiet _ = return ()
-- }}}
-- servers {{{
development = "dragongoserver.sourceforge.net"
production  = "www.dragongoserver.net"
-- }}}
-- login {{{
data Result
    = UnsuccessfulResponse (Response String)
    | WrongUsername
    | WrongPassword
    | OtherProblem String
    | Success
    deriving Show

login :: String -> String -> String -> DGS Result
login server username password = fmap munge (post loc opts) where
    loc  = uri server "login.php"
    opts = [("quick_mode", "1"), ("userid", username), ("passwd", password)]

    munge rsp = case rspCode rsp of
        (2, 0, 0) -> resultFromString . rspBody $ rsp
        _         -> UnsuccessfulResponse       $ rsp

    resultFromString "#Error: wrong_userid\n"   = WrongUsername
    resultFromString "#Error: wrong_password\n" = WrongPassword
    resultFromString "\nOk"                     = Success
    resultFromString s                          = OtherProblem s
-- }}}
-- status {{{
-- (gid, uid, black?, date, time remaining)
type Game    = (Integer, String, Bool, String, String)
-- (mid, uid, subject, date)
type Message = (Integer, String, String, String)

strip :: String -> String
strip s = read . (\s -> '"' : s ++ "\"") . take (length s - 2) . drop 1 $ s

gameFromString    :: String -> Game
messageFromString :: String -> Message
statusFromString  :: String -> ([Message], [Game])

gameFromString s = case sepBy ", " s of
    ["'G'", gid, uid, color, date, time] -> (read gid, strip uid, color == "'B'", strip date, strip time)
messageFromString s = case sepBy ", " s of
    ["'M'", mid, uid, subject, date] -> (read mid, strip uid, strip subject, strip date)
statusFromString s = (messages, games) where
    types c  = filter (isPrefixOf ('\'' : c : "', ")) (lines s)
    games    = map gameFromString    (types 'G')
    messages = map messageFromString (types 'M')

status     :: String            -> DGS ([Message], [Game])
statusUID  :: String -> Integer -> DGS [Game]
statusUser :: String -> String  -> DGS [Game]

status     server      = valueOf statusFromString         $ get  (uri server "quick_status.php")
statusUID  server uid  = valueOf (snd . statusFromString) $ post (uri server "quick_status.php") [("uid", show uid)]
statusUser server user = valueOf (snd . statusFromString) $ post (uri server "quick_status.php") [("user", user)]
-- }}}
