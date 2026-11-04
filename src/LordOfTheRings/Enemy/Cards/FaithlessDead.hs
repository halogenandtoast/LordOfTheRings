module LordOfTheRings.Enemy.Cards.FaithlessDead (faithlessDead) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Phantom))

newtype FaithlessDead = FaithlessDead EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Phantom.
faithlessDead :: EnemyCard FaithlessDead
faithlessDead = enemy FaithlessDead Cards.faithlessDead

-- | When Revealed: Either raise each player's threat by 2, or reveal an additional encounter
-- card. ("You", who reveals it, chooses.)
-- Shadow: Attacking enemy gets +1 attack and loses the Phantom keyword for this attack.
instance RunMessage FaithlessDead where
  runMessage msg e@(FaithlessDead attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealer <- getRevealer
      chooseOneM revealer do
        questionLabeled "faithlessDead.choose"
        causedBy attrs
        labeled "faithlessDead.raiseThreat" $ eachPlayer \p -> encounterRaisesThreat p 2
        labeled "faithlessDead.reveal" revealEncounterCard
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      attackModifier attrs eid (LoseKeyword Phantom)
      pure e
    _ -> FaithlessDead <$> runMessage msg attrs
