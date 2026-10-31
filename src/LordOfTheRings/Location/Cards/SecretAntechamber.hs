module LordOfTheRings.Location.Cards.SecretAntechamber (secretAntechamber) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype SecretAntechamber = SecretAntechamber LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

secretAntechamber :: LocationCard SecretAntechamber
secretAntechamber = location SecretAntechamber Cards.secretAntechamber

-- | Immune to player card effects.
-- While Secret Antechamber is in the staging area, no more than 5 progress can be placed on
-- the main quest each round.
instance HasModifiersFor SecretAntechamber where
  getModifiersFor (SecretAntechamber a) = do
    modified a a [ImmuneToPlayerCardEffects]
    when (a.placement == StagingArea) do
      mains <- getMainQuests
      for_ mains \q -> modified a q [ProgressPerRoundLimit 5]

-- | Travel: Raise each player's threat by 3 to travel here.
instance RunMessage SecretAntechamber where
  runMessage msg l@(SecretAntechamber a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> encounterRaisesThreat p 3
      pure l
    _ -> SecretAntechamber <$> runMessage msg a
