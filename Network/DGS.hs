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

gameInfo :: ID GameTag -> DGS (ID GameTag)
gameInfo (ID gid) = object "game" "info" [("gid", show gid)]

-- | you can only get private comments if you are logged in; if you are not
-- logged in, this will succeed, but a request for private comments will be
-- ignored, and you'll get an SGF with only the public comments
sgf :: ID GameTag
    -> Bool -- ^ request the private comments?
    -> Bool -- ^ for multiplayer games: include player info in each node? for other games: ignored
    -> DGS ByteString
sgf (ID gid) comments playerInfo = do
	server <- asks fst
	get id (uri server "sgf.php") opts
	where
	opts = [("gid", show gid), ("owned_comments", show . fromEnum $ comments), ("mpg", show . fromEnum . not $ playerInfo)]
