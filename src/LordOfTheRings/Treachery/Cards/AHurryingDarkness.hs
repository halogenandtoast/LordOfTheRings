module LordOfTheRings.Treachery.Cards.AHurryingDarkness (aHurryingDarkness) where

import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AHurryingDarkness = AHurryingDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aHurryingDarkness :: TreacheryCard AHurryingDarkness
aHurryingDarkness = treachery AHurryingDarkness Cards.aHurryingDarkness

-- | When Revealed: Each player must either increase his threat by 5, or reveal an
-- additional encounter card from the encounter deck and add it to the staging area.
-- Shadow: Attacking enemy gets +X attack. X is the threat of the active location.
instance RunMessage AHurryingDarkness where
  runMessage msg t@(AHurryingDarkness attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> chooseOneM pid do
        questionLabeled "aHurryingDarkness.choose"
        causedBy attrs
        labeled "aHurryingDarkness.threat" $ encounterRaisesThreat pid 5
        labeled "aHurryingDarkness.reveal" revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      active <- select IsActiveLocation
      x <- sum <$> traverse getLocationThreat active
      when (x > 0) $ enemyGetsAttack attrs eid x
      pure t
    _ -> AHurryingDarkness <$> runMessage msg attrs
