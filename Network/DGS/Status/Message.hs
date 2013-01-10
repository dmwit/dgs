module Network.DGS.Status.Message where

import Data.Attoparsec
import Data.ByteString
import Data.Time
import Network.DGS.Misc
import Network.DGS.User
import Network.DGS.Message
import Network.DGS.Status.Internal

data Category
	= Normal     -- ^ standard private message
	| Invitation -- ^ game invitation
	| Dispute    -- ^ disputed game invitation
	| Result     -- ^ game-end notification
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Message = Message
	{ mid      :: ID MessageTag
	, fid      :: ID FolderTag
	, category :: Category
	, sender   :: Nick
	, subject  :: ByteString
	, date     :: UTCTime
	} deriving (Eq, Ord, Show, Read)

instance Atto Category where
	attoparse = choice
		[ "NORMAL"     --> Normal
		, "INVITATION" --> Invitation
		, "DISPUTED"   --> Dispute
		, "RESULT"     --> Result
		]

instance Atto Message where
	attoparse = do
		word8 (enum 'M')
		mid_      <- comma >> attoparse
		fid_      <- comma >> attoparse
		category_ <- comma >> attoparse
		sender_   <- comma >> attoparse
		subject_  <- comma >> quotedField
		date_     <- comma >> attoparse
		return Message
			{ mid      = mid_
			, fid      = fid_
			, category = category_
			, sender   = sender_
			, subject  = subject_
			, date     = date_
			}
