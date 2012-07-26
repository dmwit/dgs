{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.DGS.Types where

import Control.Monad.Trans
import Data.Time
import Network.Browser
import Network.DGS.Errors
import Network.HTTP

-- | a convenient type synonym for HTTP's browser monad
newtype DGS a = DGS { runDGS :: BrowserAction (HandleStream String) a } deriving (Functor, Monad, MonadIO)

data LoginResult
	= WrongUsername
	| WrongPassword
	| LoginProblem String -- ^ it's a bug in the library if one of these ever gets built
	| LoginSuccess
	deriving (Eq, Ord, Show, Read)

data Response a
	= UnknownVersion Quota String -- ^ currently, only 1.0.15:13 is supported
	| Problem        Quota Error  -- ^ something was wrong with your request
	| Success        Quota a      -- ^ the stars aligned; here's your answer!
	deriving (Eq, Ord, Show, Read)

-- | how many accesses you have left, and when the quota will reset to its
-- maximum
data Quota = Quota Integer UTCTime deriving (Eq, Ord, Show, Read)

-- | 0-indexed x/y coordinates that start at the top left
type Point = (Integer, Integer)
