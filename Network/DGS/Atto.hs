{-# LANGUAGE NoMonomorphismRestriction #-}
module Network.DGS.Atto
	( module Control.Applicative
	, module Data.Attoparsec
	, module Network.DGS.Atto
	, ByteString
	) where

import Control.Applicative
import Data.Attoparsec hiding (Parser)
import Data.Attoparsec as A
import Data.ByteString (ByteString)
import Data.Int
import Data.Ix
import Data.SGF.Types (Color(..), RuleSetGo(..))
import Data.Time
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

integral = liftA2 (*) sign natural where
	sign = (word8 (enum '-') >> return (-1))
		<|> return 1
natural = digits2Integer <$> takeWhile1 isDigit where
	isDigit x      = x >= zero && x <= nine
	digits2Integer = W.foldl' (\n d -> n * 10 + fromIntegral (d - zero)) 0

	zero = enum '0'
	nine = enum '9'

column = comma >> attoparse

bracketed s e p = do
	word8 (enum s)
	v <- p
	word8 (enum e)
	return v

parenthesized = bracketed '(' ')'
quoted        = bracketed '\'' '\''

class Atto a where attoparse :: A.Parser a
instance Atto Integer where attoparse = integral

instance Atto Int16 where
	attoparse = attoparse >>= \n -> if inRange (-32768,32767) n then return (fromInteger n) else fail $ "number out of range for an Int16: " ++ show n

instance Atto UTCTime where
	attoparse = do
		contents <- quotedField
		case parseTime defaultTimeLocale "%F %T" (C.unpack contents) of
			Just  t -> return t
			Nothing -> fail $ "couldn't parse date " ++ C.unpack contents

instance Atto Bool      where attoparse = "0"       --> False   <|> "1"        --> True
instance Atto Color     where attoparse = "B"       --> Black   <|> "W"        --> White
instance Atto RuleSetGo where attoparse = "CHINESE" --> Chinese <|> "JAPANESE" --> Japanese
