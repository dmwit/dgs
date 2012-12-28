{-# LANGUAGE EmptyDataDecls, FlexibleInstances #-}
module Network.DGS.Bulletin where

import Data.Aeson
import Network.DGS.Misc

-- | for use with 'ID'
data BulletinTag
instance FromJSON (ID BulletinTag) where parseJSON = parseID "bulletin"
