module LordOfTheRings.Enemy.Cards.BellachsScout (bellachsScout) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (isScour, placeDamageOnActiveLocation)

newtype BellachsScout = BellachsScout EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bellachsScout :: EnemyCard BellachsScout
bellachsScout = enemy BellachsScout Cards.bellachsScout

-- | When Revealed: Either place 3 damage on the active location, or reveal an additional
-- encounter card. The first player chooses; with no active location only the reveal is left.
-- Scour: Place 1 damage on the active location.
-- Shadow: Place 1 damage on the active location.
instance RunMessage BellachsScout where
  runMessage msg e@(BellachsScout attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      active <- selectAny IsActiveLocation
      chooseOrRunOneM lead do
        questionLabeled "bellachsScout.choose"
        causedBy attrs
        when active $ labeled "bellachsScout.damage" $ placeDamageOnActiveLocation attrs 3
        labeled "bellachsScout.reveal" revealEncounterCard
      pure e
    _ | isScour attrs msg -> do
      placeDamageOnActiveLocation attrs 1
      pure e
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      placeDamageOnActiveLocation (CardIdSource cid) 1
      pure e
    _ -> BellachsScout <$> runMessage msg attrs
