module LordOfTheRings.Enemy.Cards.GoblinTormentor (goblinTormentor) where

import LordOfTheRings.Card (toCard)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (capturedUnder, cardsWithCaptured)

newtype GoblinTormentor = GoblinTormentor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinTormentor :: EnemyCard GoblinTormentor
goblinTormentor = enemy GoblinTormentor Cards.goblinTormentor

-- | X is the number of cards in play with 1 or more captured cards underneath it.
instance HasModifiersFor GoblinTormentor where
  getModifiersFor (GoblinTormentor a) = do
    x <- cardsWithCaptured
    modified a a [AttackModifier x | x > 0]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if it has no captured cards
-- underneath it).
instance RunMessage GoblinTormentor where
  runMessage msg e@(GoblinTormentor attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      held <- capturedUnder . (.id) . toCard =<< getEnemy eid
      enemyGetsAttack attrs eid (if null held then 2 else 1)
      pure e
    _ -> GoblinTormentor <$> runMessage msg attrs
