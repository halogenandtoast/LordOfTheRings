module LordOfTheRings.Enemy.Cards.UrukProwler (urukProwler) where

import LordOfTheRings.EncounterSet (EncounterSet (MordorOrcs))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (discardRandomSetAsideEnemyLabeled)

newtype UrukProwler = UrukProwler EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukProwler :: EnemyCard UrukProwler
urukProwler = enemy UrukProwler Cards.urukProwler

{- | When Revealed: Either place a random enemy from the set aside Mordor Orcs set in the
encounter discard pile, or Uruk Prowler makes an immediate attack against the first
player. The first player decides.
Shadow: Discard a random card from your hand.
-}
instance RunMessage UrukProwler where
  runMessage msg e@(UrukProwler attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "urukProwler.choose"
        causedBy attrs
        discardRandomSetAsideEnemyLabeled "urukProwler.discard" MordorOrcs
        labeled "urukProwler.attack" $ immediateAttack lead attrs.id
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardRandomFromHand pid 1
      pure e
    _ -> UrukProwler <$> runMessage msg attrs
