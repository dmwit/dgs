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

module Network.DGS where

import Data.ByteString.Lazy
import Network.DGS.Misc
import Network.DGS.Game
import Network.DGS.Monad

gameInfo :: String -> ID GameTag -> DGS (Response (ID GameTag))
gameInfo server (ID gid) = object "game" "info" [("gid", show gid)] server

-- TODO: update this to take advantage of all the new multi-player game stuff
-- from the new specs.
--
-- | you can only get private comments if you are logged in; if you are not
-- logged in, this will succeed, but a request for private comments will be
-- ignored, and you'll get an SGF with only the public comments
sgf :: String  -- ^ server
    -> Integer -- ^ game ID
    -> Bool    -- ^ request the private comments?
    -> DGS ByteString
sgf server gid comments = get id (uri server "sgf.php") opts where
    opts = [("gid", show gid), ("owned_comments", show . fromEnum $ comments)]
