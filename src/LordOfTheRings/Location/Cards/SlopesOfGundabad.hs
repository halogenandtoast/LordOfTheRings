module LordOfTheRings.Location.Cards.SlopesOfGundabad (slopesOfGundabad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (assaultProgressLabeled)

newtype SlopesOfGundabad = SlopesOfGundabad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

slopesOfGundabad :: LocationCard SlopesOfGundabad
slopesOfGundabad = location SlopesOfGundabad Cards.slopesOfGundabad

-- | When Revealed: Either place 1 progress on The Goblins' Assault, or reveal an additional
-- encounter card. The first player chooses.
-- Shadow: Attacking enemy cannot take damage this phase.
instance RunMessage SlopesOfGundabad where
  runMessage msg l@(SlopesOfGundabad a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "slopesOfGundabad.choose"
        causedBy a
        assaultProgressLabeled a
        labeled "slopesOfGundabad.reveal" revealEncounterCard
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      phaseModifier (CardIdSource cid) eid CannotTakeDamage
      pure l
    _ -> SlopesOfGundabad <$> runMessage msg a
