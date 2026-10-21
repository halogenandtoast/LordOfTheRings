module LordOfTheRings.Location.Cards.BeePastures (beePastures) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives

newtype BeePastures = BeePastures LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

beePastures :: LocationCard BeePastures
beePastures = location BeePastures Cards.beePastures

-- | Response: After you travel to Bee Pastures, search the encounter deck and discard pile
-- for Grimbeorn the Old and add him to the staging area. Then, shuffle the encounter deck.
instance HasAbilities BeePastures where
  getAbilities (BeePastures a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

instance RunMessage BeePastures where
  runMessage msg l@(BeePastures a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      push $ SearchEncounterDeckAndDiscard pid (toTarget a) (cardIs Objectives.grimbeornTheOld)
      pure l
    _ -> BeePastures <$> runMessage msg a
