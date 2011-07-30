{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.DGS.Types where

-- TODO: remove this imports when the instance is available from the HTTP package
import Control.Monad.Trans
import Data.Array
import Network.Browser
import Network.HTTP

-- | a convenient type synonym for HTTP's browser monad
newtype DGS a = DGS { runDGS :: BrowserAction (HandleStream String) a } deriving (Functor, Monad, MonadIO)

-- TODO: port this upstream to the HTTP package
instance MonadIO (BrowserAction conn) where liftIO = ioAction

data LoginResult
    = WrongUsername
    | WrongPassword
    | LoginProblem String -- ^ it's a bug in the library if one of these ever gets built
    | LoginSuccess
    deriving (Eq, Ord, Show, Read)

-- | black?
type Color = Bool

-- | (message ID, username of the sender, subject, date)
data Message = Message {
    mid         :: Integer, -- ^ the unique message ID
    sender      :: String,  -- ^ username
    subject     :: String,
    date        :: String
    }

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

data User = User {
    name        :: String, -- ^ real-life name
    nick        :: String, -- ^ username
    time        :: String,
    prisoners   :: Integer
    }

data GameBoard = GameBoard {
    message     :: String,                      -- ^ message, if you're logged in; an empty string means you aren't logged in or there was no message
    notes       :: Maybe String,                -- ^ game notes, if you are logged in and they're currently showing
    board       :: Array Point (Maybe Color),
    move        :: Integer,
    black       :: User,
    white       :: User,
    ruleset     :: String,
    komi        :: Rational,
    handicap    :: Integer,
    rated       :: Bool,
    timeLimit   :: String
    }
