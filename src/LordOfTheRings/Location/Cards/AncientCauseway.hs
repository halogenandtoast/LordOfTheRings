module LordOfTheRings.Location.Cards.AncientCauseway (ancientCauseway) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype AncientCauseway = AncientCauseway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ancientCauseway :: LocationCard AncientCauseway
ancientCauseway = location AncientCauseway Cards.ancientCauseway

-- | Forced: When Ancient Causeway is explored, raise each player's threat by 2.
instance HasAbilities AncientCauseway where
  getAbilities (AncientCauseway a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage AncientCauseway where
  runMessage msg l@(AncientCauseway a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 2
      pure l
    _ -> AncientCauseway <$> runMessage msg a
