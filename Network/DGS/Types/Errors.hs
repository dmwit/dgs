module Network.DGS.Types.Errors where

import Data.Maybe

-- | Convert a type of error to the corresponding category of errors. Some types
--   of errors fall under several categories.
kind        :: Label  -> [Kind]

-- | List all the types of errors from a particular category.
labels      :: Kind   -> [Label]

-- | Convert a type of error to its JSON representation.
jsonName    :: Label  -> String

-- | Convert a JSON string to the corresponding type of error.
label       :: String -> Maybe Label

-- | Convenience function wrapping 'label'.
unsafeLabel :: String -> Label

-- | Errors include a type of error and a string with error details.
data Error
	= KnownError   Label  String
	| UnknownError String String -- ^ it's a bug in the library if one of these ever gets built
	deriving (Eq, Ord, Show, Read)

-- | Extract just the \"details\" part of an error -- that is, the second
-- field of each constructor.
details :: Error -> String
details (KnownError   _ s) = s
details (UnknownError _ s) = s

kind l = case l of
	NotLoggedIn          -> [LoginKind]
	WrongUserid          -> [LoginKind]
	WrongPassword        -> [LoginKind]
	CookiesDisabled      -> [LoginKind]
	FeverVault           -> [LoginKind]
	LoginDenied          -> [LoginKind]
	IpBlockedGuestLogin  -> [LoginKind]
	ServerDown           -> [GeneralKind]
	InvalidCommand       -> [GeneralKind]
	InvalidArgs          -> [GeneralKind]
	UnknownEntry         -> [GeneralKind]
	MysqlQueryFailed     -> [GeneralKind]
	MailFailure          -> [GeneralKind]
	UnknownRuleset       -> [GameCreationKind]
	HandicapRange        -> [GameCreationKind]
	KomiRange            -> [GameCreationKind]
	InvalidSnapshot      -> [GameCreationKind]
	InvalidSnapshotChar  -> [GameCreationKind]
	MismatchSnapshot     -> [GameCreationKind]
	NoInitialRating      -> [GameCreationKind]
	TimeLimitTooSmall    -> [GameCreationKind]
	IllegalPosition      -> [GameCreationKind]
	UnknownGame          -> [GameCreationKind, GameKind]
	InvitedToUnknownGame -> [GameCreationKind]
	GameAlreadyAccepted  -> [GameCreationKind]
	WrongDisputeGame     -> [GameCreationKind]
	WrongPlayers         -> [GameCreationKind]
	MysqlStartGame       -> [GameCreationKind]
	MysqlDataCorruption  -> [GameCreationKind]
	FeatureDisabled      -> [GameCreationKind]
	InternalError        -> [GameCreationKind, MoveKind]
	GameNotStarted       -> [GameKind]
	GameFinished         -> [GameKind]
	InvalidGameStatus    -> [GameKind]
	DatabaseCorrupted    -> [GameKind]
	AlreadyPlayed        -> [GameKind]
	NotGamePlayer        -> [GameKind]
	InvalidCoord         -> [MoveKind]
	MoveProblem          -> [MoveKind]
	NotYourTurn          -> [MoveKind]
	InvalidAction        -> [MoveKind, MessageSendKind]
	MysqlUpdateGame      -> [MoveKind]
	MysqlInsertMove      -> [MoveKind]
	OpponentNotFound     -> [MoveKind]
	ReceiverNotFound     -> [MoveKind, MessageGetKind]
	MysqlInsertMessage   -> [MoveKind]
	UnknownMessage       -> [MessageGetKind]
	FolderNotFound       -> [MessageGetKind]
	BulkmessageSelf      -> [MessageGetKind]
	ReplyInvalid         -> [MessageSendKind]
	FolderForbidden      -> [MessageSendKind]
	GameDeleteInvitation -> [MessageSendKind]

labels k = case k of
	LoginKind        -> [NotLoggedIn, WrongUserid, WrongPassword, CookiesDisabled, FeverVault, LoginDenied, IpBlockedGuestLogin]
	GeneralKind      -> [ServerDown, InvalidCommand, InvalidArgs, UnknownEntry, MysqlQueryFailed, MailFailure]
	GameCreationKind -> [UnknownRuleset, HandicapRange, KomiRange, InvalidSnapshot, InvalidSnapshotChar, MismatchSnapshot, NoInitialRating, TimeLimitTooSmall, IllegalPosition, UnknownGame, InvitedToUnknownGame, GameAlreadyAccepted, WrongDisputeGame, WrongPlayers, MysqlStartGame, MysqlDataCorruption, FeatureDisabled, InternalError]
	GameKind         -> [UnknownGame, GameNotStarted, GameFinished, InvalidGameStatus, DatabaseCorrupted, AlreadyPlayed, NotGamePlayer]
	MoveKind         -> [InvalidCoord, MoveProblem, NotYourTurn, InternalError, InvalidAction, MysqlUpdateGame, MysqlInsertMove, OpponentNotFound, ReceiverNotFound, MysqlInsertMessage]
	MessageGetKind   -> [UnknownMessage, ReceiverNotFound, FolderNotFound, BulkmessageSelf]
	MessageSendKind  -> [InvalidAction, ReplyInvalid, FolderForbidden, GameDeleteInvitation]

jsonName l = case l of
	NotLoggedIn          -> "not_logged_in"
	WrongUserid          -> "wrong_userid"
	WrongPassword        -> "wrong_password"
	CookiesDisabled      -> "cookies_disabled"
	FeverVault           -> "fever_vault"
	LoginDenied          -> "login_denied"
	IpBlockedGuestLogin  -> "ip_blocked_guest_login"
	ServerDown           -> "server_down"
	InvalidCommand       -> "invalid_command"
	InvalidArgs          -> "invalid_args"
	UnknownEntry         -> "unknown_entry"
	MysqlQueryFailed     -> "mysql_query_failed"
	MailFailure          -> "mail_failure"
	UnknownRuleset       -> "unknown_ruleset"
	HandicapRange        -> "handicap_range"
	KomiRange            -> "komi_range"
	InvalidSnapshot      -> "invalid_snapshot"
	InvalidSnapshotChar  -> "invalid_snapshot_char"
	MismatchSnapshot     -> "mismatch_snapshot"
	NoInitialRating      -> "no_initial_rating"
	TimeLimitTooSmall    -> "time_limit_too_small"
	IllegalPosition      -> "illegal_position"
	InvitedToUnknownGame -> "invited_to_unknown_game"
	GameAlreadyAccepted  -> "game_already_accepted"
	WrongDisputeGame     -> "wrong_dispute_game"
	WrongPlayers         -> "wrong_players"
	MysqlStartGame       -> "mysql_start_game"
	MysqlDataCorruption  -> "mysql_data_corruption"
	FeatureDisabled      -> "feature_disabled"
	UnknownGame          -> "unknown_game"
	GameNotStarted       -> "game_not_started"
	GameFinished         -> "game_finished"
	InvalidGameStatus    -> "invalid_game_status"
	DatabaseCorrupted    -> "database_corrupted"
	AlreadyPlayed        -> "already_played"
	NotGamePlayer        -> "not_game_player"
	InvalidCoord         -> "invalid_coord"
	MoveProblem          -> "move_problem"
	NotYourTurn          -> "not_your_turn"
	InternalError        -> "internal_error"
	MysqlUpdateGame      -> "mysql_update_game"
	MysqlInsertMove      -> "mysql_insert_move"
	OpponentNotFound     -> "opponent_not_found"
	MysqlInsertMessage   -> "mysql_insert_message"
	UnknownMessage       -> "unknown_message"
	ReceiverNotFound     -> "receiver_not_found"
	FolderNotFound       -> "folder_not_found"
	BulkmessageSelf      -> "bulkmessage_self"
	InvalidAction        -> "invalid_action"
	ReplyInvalid         -> "reply_invalid"
	FolderForbidden      -> "folder_forbidden"
	GameDeleteInvitation -> "game_delete_invitation"

label s = case s of
	"not_logged_in"           -> Just NotLoggedIn
	"wrong_userid"            -> Just WrongUserid
	"wrong_password"          -> Just WrongPassword
	"cookies_disabled"        -> Just CookiesDisabled
	"fever_vault"             -> Just FeverVault
	"login_denied"            -> Just LoginDenied
	"ip_blocked_guest_login"  -> Just IpBlockedGuestLogin
	"server_down"             -> Just ServerDown
	"invalid_command"         -> Just InvalidCommand
	"invalid_args"            -> Just InvalidArgs
	"unknown_entry"           -> Just UnknownEntry
	"mysql_query_failed"      -> Just MysqlQueryFailed
	"mail_failure"            -> Just MailFailure
	"unknown_ruleset"         -> Just UnknownRuleset
	"handicap_range"          -> Just HandicapRange
	"komi_range"              -> Just KomiRange
	"invalid_snapshot"        -> Just InvalidSnapshot
	"invalid_snapshot_char"   -> Just InvalidSnapshotChar
	"mismatch_snapshot"       -> Just MismatchSnapshot
	"no_initial_rating"       -> Just NoInitialRating
	"time_limit_too_small"    -> Just TimeLimitTooSmall
	"illegal_position"        -> Just IllegalPosition
	"invited_to_unknown_game" -> Just InvitedToUnknownGame
	"game_already_accepted"   -> Just GameAlreadyAccepted
	"wrong_dispute_game"      -> Just WrongDisputeGame
	"wrong_players"           -> Just WrongPlayers
	"mysql_start_game"        -> Just MysqlStartGame
	"mysql_data_corruption"   -> Just MysqlDataCorruption
	"feature_disabled"        -> Just FeatureDisabled
	"unknown_game"            -> Just UnknownGame
	"game_not_started"        -> Just GameNotStarted
	"game_finished"           -> Just GameFinished
	"invalid_game_status"     -> Just InvalidGameStatus
	"database_corrupted"      -> Just DatabaseCorrupted
	"already_played"          -> Just AlreadyPlayed
	"not_game_player"         -> Just NotGamePlayer
	"invalid_coord"           -> Just InvalidCoord
	"move_problem"            -> Just MoveProblem
	"not_your_turn"           -> Just NotYourTurn
	"internal_error"          -> Just InternalError
	"mysql_update_game"       -> Just MysqlUpdateGame
	"mysql_insert_move"       -> Just MysqlInsertMove
	"opponent_not_found"      -> Just OpponentNotFound
	"mysql_insert_message"    -> Just MysqlInsertMessage
	"unknown_message"         -> Just UnknownMessage
	"receiver_not_found"      -> Just ReceiverNotFound
	"folder_not_found"        -> Just FolderNotFound
	"bulkmessage_self"        -> Just BulkmessageSelf
	"invalid_action"          -> Just InvalidAction
	"reply_invalid"           -> Just ReplyInvalid
	"folder_forbidden"        -> Just FolderForbidden
	"game_delete_invitation"  -> Just GameDeleteInvitation
	_ -> Nothing

unsafeLabel s = fromMaybe (error ("unsafeLabel applied to unknown JSON error name: " ++ s)) (label s)

-- | Other places in the documentation refer to specific kinds of errors that
-- can be reported by DGS.
data Kind
	= LoginKind
	| GeneralKind
	| GameCreationKind
	| GameKind
	| MoveKind
	| MessageGetKind
	| MessageSendKind
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | A comprehensive list of the types of errors that DGS will report; the
-- descriptions of each below are copied directly from the specification.
-- (TODO: specialize these descriptions to refer to the Haskell bindings more
-- specifically.)
data Label
	= NotLoggedIn          -- ^ user is not logged in
	| WrongUserid          -- ^ unknown user-id provided for login
	| WrongPassword        -- ^ wrong password to login given user
	| CookiesDisabled      -- ^ no cookies set with user-id and login-session
	| FeverVault           -- ^ login quota exceeded
	| LoginDenied          -- ^ login of user denied by admin
	| IpBlockedGuestLogin  -- ^ IP-range is blocked for login as guest-user
	| ServerDown           -- ^ server in maintenance-mode
	| InvalidCommand       -- ^ invalid command for quick-suite
	| InvalidArgs          -- ^ missing or invalid arguments given
	| UnknownEntry         -- ^ entry identified by id could not be found
	| MysqlQueryFailed     -- ^ database query failed
	| MailFailure          -- ^ sending mail for notification failed (e.g. for quota-exceeded)
	| UnknownRuleset       -- ^ unknown ruleset (known: JAPANESE, CHINESE)
	| HandicapRange        -- ^ invalid handicap specified
	| KomiRange            -- ^ invalid komi specified
	| InvalidSnapshot      -- ^ snapshot for shape-game is invalid
	| InvalidSnapshotChar  -- ^ invalid character to represent shape-snapshot used
	| MismatchSnapshot     -- ^ shape-game inconsistency with snapshot detected
	| NoInitialRating      -- ^ player needs rating for required handicap-type
	| TimeLimitTooSmall    -- ^ given time is too small
	| IllegalPosition      -- ^ illegal position detected on loading game-board
	| UnknownGame          -- ^ 'GameCreationKind': game-data can not be found; 'GameKind': invalid game
	| InvitedToUnknownGame -- ^ game invited to is unknown to system
	| GameAlreadyAccepted  -- ^ game has already been accepted, probable race-condition
	| WrongDisputeGame     -- ^ players do not match with dispute-game
	| WrongPlayers         -- ^ players do not match with game-request
	| MysqlStartGame       -- ^ database-error on starting game
	| MysqlDataCorruption  -- ^ data is corrupted -> contact admin
	| FeatureDisabled      -- ^ feature (probably tournament) is disabled
	| InternalError        -- ^ 'GameCreationKind': data is inconsistent -> contact admin; 'MoveKind': board-data could not be loaded -> contact admin
	| GameNotStarted       -- ^ game has not started yet (still in invitation-mode)
	| GameFinished         -- ^ game is already finished (move/operation not possible)
	| InvalidGameStatus    -- ^ game is in wrong status to perform requested operation
	| DatabaseCorrupted    -- ^ player-to-move can not be determined -> contact admin
	| AlreadyPlayed        -- ^ @moveId@ context for current move in game is no longer valid, or was wrong in the first place
	| NotGamePlayer        -- ^ you are not a player of the game
	| InvalidCoord         -- ^ invalid coordinates given
	| MoveProblem          -- ^ @pass@-move only allowed for @move@-command
	| NotYourTurn          -- ^ it is not your turn to move in the game; (error not occuring for commands @delete@, @resign@)
	| InvalidAction        -- ^ 'MoveKind': unknown action; 'MessageSendKind': failure on checking to send a message, detailed error-texts found in @error_texts@-field
	| MysqlUpdateGame      -- ^ database query to update game failed
	| MysqlInsertMove      -- ^ database query to insert move failed
	| OpponentNotFound     -- ^ database corrupt regarding game-opponent -> contact admin
	| ReceiverNotFound     -- ^ 'MoveKind': message-receiver for notify could not be found -> contact admin; 'MessageGetKind': given recipient(s) can not be found
	| MysqlInsertMessage   -- ^ database query to insert message for notify failed
	| UnknownMessage       -- ^ unknown message-id specified
	| FolderNotFound       -- ^ specified folder is unknown for current user
	| BulkmessageSelf      -- ^ bulk-messages can not be sent to oneself
	| ReplyInvalid         -- ^ invalid reply, because ...
	                       --
	                       -- * message can't be replied to, or no reply possible for message
	                       --
	                       -- * reply to myself is not allowed (as that doesn't show up in message-thread-view)
	                       --
	                       -- * for normal messages (can be ok for accepting/declining invitation)-- ^
	                       --   reply only possible to NORMAL-messages, not to invitations/disputes or system-messages
	                       --
	                       -- * for accepting/declining invitation
	                       --   reply only possible to invitation message, not to normal messages or already disputed invitation
	                       --
	                       -- * used OTHER_UID|OTHER_HANDLE arguments to specify message-recipient,
	                       --   recipient must be omitted as it's determined from message-sender
	| FolderForbidden      -- ^ invalid target folder used to move replied message to
	                       --
	                       -- * folder-id must be >0
	                       --
	                       -- * NEW-folder (folder-id=2) and SENT-folder (folder-id=5) are not allowed for old message to store into
	| GameDeleteInvitation -- ^ error on game-deletion/decline-invitation detected,
	                       -- nothing found to be deleted, which could mean that the invitation was already declined
	deriving (Eq, Ord, Show, Read, Enum, Bounded)
