{-# LANGUAGE NoMonomorphismRestriction #-}
module Network.DGS.Status.Internal
	( module Network.DGS.Status.Internal
	, word8
	) where

import Control.Applicative
import Data.Attoparsec as A
import Data.ByteString (ByteString)
import Data.Text.Encoding
import Data.Time
import Network.DGS.Game (Style(..))
import Network.DGS.Misc
import Network.DGS.User
import System.Locale
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString       as W

enum    = toEnum . fromEnum
comma   = word8 (enum ',')
s --> v = string (C.pack s) >> return v

field = quotedField <|> A.takeWhile (not . special)
	where special x = x `elem` map enum "'\\ ,"

quotedField = do
	word8 (enum '\'')
	chunks <- many (chunk <|> escape)
	word8 (enum '\'')
	return (W.concat chunks)
	where
	chunk  = takeWhile1 (not . special)
	escape = word8 (enum '\\') >> pack <$> satisfy special

	special x = x == enum '\'' || x == enum '\\'
	pack    x = W.pack [x]

natural = digits2Integer <$> takeWhile1 isDigit where
	isDigit x      = x >= zero && x <= nine
	digits2Integer = W.foldl' (\n d -> n * 10 + fromIntegral (d - zero)) 0

	zero = enum '0'
	nine = enum '9'

column = comma >> attoparse

class Atto a where attoparse :: Parser a
instance Atto (ID a) where attoparse = ID <$> natural

instance Atto Integer where
	attoparse = liftA2 (*) sign natural where
		sign = (word8 (enum '-') >> return (-1))
		    <|> return 1

instance Atto UTCTime where
	attoparse = do
		contents <- quotedField
		case parseTime defaultTimeLocale "%F %T" (C.unpack contents) of
			Just  t -> return t
			Nothing -> fail $ "couldn't parse date " ++ C.unpack contents

-- nicks can only contain a-z, A-Z, 0-9, and -_+, which are pretty
-- much the same in all encodings, so just take a stab at one
instance Atto Nick where attoparse = Nick . decodeUtf8 <$> quotedField

instance Atto Style where
	attoparse = go <|> teamGo <|> zenGo where
		go     = string (C.pack "GO") >> return Plain
		teamGo = do
			string (C.pack "TEAM_GO(")
			n <- natural
			word8 (enum ':')
			m <- natural
			word8 (enum ')')
			return (Team n m)
		zenGo  = do
			string (C.pack "ZEN_GO(")
			n <- natural
			word8 (enum ')')
			return (Zen n)
