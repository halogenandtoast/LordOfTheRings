module LordOfTheRings.Name (Name (..)) where

import LordOfTheRings.Prelude

data Name = Name Text (Maybe Text)
  deriving stock (Show, Eq, Generic)

instance IsString Name where
  fromString s = Name (fromString s) Nothing

instance ToJSON Name where
  toJSON (Name title subtitle) = String $ maybe title (\s -> title <> ": " <> s) subtitle

instance FromJSON Name where
  parseJSON = withText "Name" $ \t -> pure $ Name t Nothing
