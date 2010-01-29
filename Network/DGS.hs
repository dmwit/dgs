-- boilerplate {{{
{-|
This is a quick and dirty interface to Dragon Go Server's robot interface, as
outlined at <http://www.dragongoserver.net/faq.php?read=t&cat=215#Entry219>.
It does almost no sanity-checking of things you send it, nor does it do very
much error-checking on the things Dragon sends back.  Use with caution.

Here are some sample interactions from ghci, with a fictitious password:

> *Network.DGS> browse (silence >> login development "smartypants" "password")
> LoginSuccess
> *Network.DGS> browse (silence >> statusUID production 4155) >>= mapM_ print
> (453881,"jedge42",False,"2009-12-21 03:14 GMT","F: 30d 1h")
> (532927,"bartnix",False,"2009-12-20 06:06 GMT","F: 21d 13h")
> *Network.DGS> browse (silence >> statusUser production "dmwit") >>= mapM_ print
> (453881,"jedge42",False,"2009-12-21 03:14 GMT","F: 30d 1h")
> (532927,"bartnix",False,"2009-12-20 06:06 GMT","F: 21d 13h")
> *Network.DGS> :{
> *Network.DGS| browse $ do {
> *Network.DGS|   silence;
> *Network.DGS|   login development "smartypants" "password";
> *Network.DGS|   (_, (gid, _, black, _, _):_) <- status development;
> *Network.DGS|   move development gid black (16, 18) (17, 16)
> *Network.DGS|   }
> *Network.DGS| :}
> MoveSuccess
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.DGS (
    -- * Logging in
    LoginResult(..),
    login,

    -- * Listing games and messages
    Game,
    Message,
    status,
    statusUID,
    statusUser,

    -- * Making a move
    Point,
    MoveResult(..),
    move,
    sgf,

    -- * Miscellaneous
    module Network.Browser,
    DGS(..),
    development,
    production,
    silence,
    browse
) where

-- TODO: remove this imports when the instance is available from the HTTP package
import Control.Monad.Trans
import Data.List
import Data.List.Split
import Network.Browser hiding (browse)
import Network.HTTP
import Network.URI
import qualified Network.Browser as B
-- }}}
-- helpers {{{
-- | a convenient type synonym for HTTP's browser monad
newtype DGS a = DGS { runDGS :: BrowserAction (HandleStream String) a } deriving (Functor, Monad, MonadIO)

-- TODO: port this upstream to the HTTP package
instance MonadIO (BrowserAction conn) where liftIO = ioAction

uri :: String -> String -> URI
uri server path = full where
    auth = URIAuth { uriRegName = server, uriUserInfo = "", uriPort = "" }
    full = nullURI { uriScheme = "http:", uriAuthority = Just auth, uriPath = '/' : path }

get :: (String -> a) -> URI -> [(String, String)] -> DGS a
get f uri = DGS . fmap (f . rspBody . snd) . request . formToRequest . Form GET uri

-- | by default, HTTP's browser chatters a lot on stdout; this action turns off
-- the chatter
silence :: DGS ()
silence = DGS $ setErrHandler quiet >> setOutHandler quiet where quiet _ = return ()

browse :: DGS a -> IO a
browse = B.browse . runDGS
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
data LoginResult
    = WrongUsername
    | WrongPassword
    | LoginProblem String -- ^ it's a bug in the library if one of these ever gets built
    | LoginSuccess
    deriving (Eq, Ord, Show, Read)

-- | some commands either require you to be logged in, or will give additional
-- information if you log in
login :: String -- ^ server, e.g. 'development' or 'production'
      -> String -- ^ user name
      -> String -- ^ password
      -> DGS LoginResult
login server username password = get resultFromString loc opts where
    loc  = uri server "login.php"
    opts = [("quick_mode", "1"), ("userid", username), ("passwd", password)]

    resultFromString s = case s of
        "#Error: wrong_userid\n"   -> WrongUsername
        "#Error: wrong_password\n" -> WrongPassword
        "\nOk"                     -> LoginSuccess
        _                          -> LoginProblem s
-- }}}
-- status {{{
-- | (game ID, username of the opponent, current player is black?, date, time remaining)
type Game    = (Integer, String, Bool, String, String)
-- | (message ID, username of the sender, subject, date)
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

-- | get the inbox and games list of whoever is currently logged in; this will
-- return @([], [])@ if you are not logged in
status      :: String -- ^ server
            -> DGS ([Message], [Game])
-- | get the games list of an arbitrary user; this will give the same results
-- whether or not you are logged in
statusUID   :: String  -- ^ server
            -> Integer -- ^ user ID
            -> DGS [Game]
-- | get the games list of an arbitrary user this will give the same results
-- whether or not you are logged in
statusUser  :: String  -- ^ server
            -> String  -- ^ user name
            -> DGS [Game]

status     server      = get        statusFromString  (uri server "quick_status.php") []
statusUID  server uid  = get (snd . statusFromString) (uri server "quick_status.php") [("uid", show uid)]
statusUser server user = get (snd . statusFromString) (uri server "quick_status.php") [("user", user)]
-- }}}
-- play {{{
-- | 0-indexed x/y coordinates that start at the top left
type Point = (Integer, Integer)

data MoveResult
    = NotLoggedIn
    | NoGameNumber
    | DatabaseCorrupted     -- ^ or a bad game ID
    | NotYourTurn           -- ^ or you're not playing in the game, or you claimed to be the wrong color
    | MoveAlreadyPlayed     -- ^ or the previous move didn't match reality
    | IllegalPosition       -- ^ ko, playing on top of another stone, playing off the board
    | MoveProblem String    -- ^ it's a bug in the library if one of these ever gets built
    | MoveSuccess
    deriving (Eq, Ord, Show, Read)

move :: String  -- ^ server
     -> Integer -- ^ game ID
     -> Bool    -- ^ playing as black? (can use exactly the value you got from 'status')
     -> Point   -- ^ the move the opponent just made
     -> Point   -- ^ your move
     -> DGS MoveResult
move server gid black old new = get resultFromString loc opts where
    loc  = uri server "quick_play.php"
    opts = [("gid", show gid), ("color", col), ("sgf_prev", point old), ("sgf_move", point new)]
    col  = if black then "B" else "W"
    point (x, y) = [pos x, pos y]
    pos   i      = toEnum (fromEnum 'a' + fromEnum i)

    resultFromString s | "#Error: " `isPrefixOf` s = case drop 8 s of
        "not_logged_in\n"       -> NotLoggedIn
        "no_game_nr\n"          -> NoGameNumber
        "database_corrupted\n"  -> DatabaseCorrupted
        "not_your_turn\n"       -> NotYourTurn
        "already_played\n"      -> MoveAlreadyPlayed
        "illegal_position\n"    -> IllegalPosition
    resultFromString "\nOk" = MoveSuccess
    resultFromString s      = MoveProblem s
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
