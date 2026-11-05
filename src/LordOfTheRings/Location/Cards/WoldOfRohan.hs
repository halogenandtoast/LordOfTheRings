module LordOfTheRings.Location.Cards.WoldOfRohan (woldOfRohan) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype WoldOfRohan = WoldOfRohan LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

woldOfRohan :: LocationCard WoldOfRohan
woldOfRohan = location WoldOfRohan Cards.woldOfRohan

instance HasAbilities WoldOfRohan where
  getAbilities (WoldOfRohan a) = [anyPlayer $ mkAbility a 1 $ ActionAbility (IncreasePursuitCost 1)]

instance RunMessage WoldOfRohan where
  runMessage msg l@(WoldOfRohan a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgress a a 1
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      discardNonObjectiveAttachment a pid
      pure l
    _ -> WoldOfRohan <$> runMessage msg a
