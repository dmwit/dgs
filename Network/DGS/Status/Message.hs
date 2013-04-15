module Network.DGS.Status.Message where

import Network.DGS.Status.Imports

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
	attoparse = "M" --> Message
		<*> column
		<*> column
		<*> column
		<*> column
		<*> (comma >> quotedField)
		<*> column
