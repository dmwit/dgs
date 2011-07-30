module Network.DGS.Game where

import Network.DGS.Types

data Game = Game {
    gid         :: Integer, -- ^ the unique game ID
    opponent    :: String,  -- ^ username
    color       :: Color,   -- ^ of the current player
    date        :: String,  -- ^ of the last play
    time        :: String,  -- ^ remaining for this move
    move        :: Maybe Integer,   -- ^ currently only returned by dev-server queries
    tournament  :: Maybe Integer    -- ^ currently only returned by dev-server queries
    }
