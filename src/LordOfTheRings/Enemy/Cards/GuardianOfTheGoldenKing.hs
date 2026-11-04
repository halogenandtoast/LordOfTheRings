module LordOfTheRings.Enemy.Cards.GuardianOfTheGoldenKing (guardianOfTheGoldenKing) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GuardianOfTheGoldenKing = GuardianOfTheGoldenKing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

guardianOfTheGoldenKing :: EnemyCard GuardianOfTheGoldenKing
guardianOfTheGoldenKing = enemy GuardianOfTheGoldenKing Cards.guardianOfTheGoldenKing

-- | Forced: After attackers are declared during an attack against Guardian of the Golden
-- King, look at the bottom card of the first player's deck. Choose and remove characters
-- from the attack until only X or fewer characters are attacking, where X is the
-- looked-at card's printed cost (those characters do not ready).
instance HasAbilities GuardianOfTheGoldenKing where
  getAbilities (GuardianOfTheGoldenKing a) = [mkAbility a 1 $ forced $ AttackersDeclaredWindow After a.id]

-- | The first player chooses the characters removed, one at a time.
instance RunMessage GuardianOfTheGoldenKing where
  runMessage msg e@(GuardianOfTheGoldenKing attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      bottom <- lastMay . (.deck) <$> getPlayer lead
      attackers <- selectCharacters AttackingHero AttackingAlly
      doStep (length attackers - maybe 0 printedCostOf bottom) msg
      pure e
    DoStep k inner@(UseCardAbility _ (isSource attrs -> True) 1 _) | k > 0 -> do
      lead <- getLead
      attackers <- selectCharacters AttackingHero AttackingAlly
      chooseOrRunOneM lead do
        questionLabeled "guardianOfTheGoldenKing.remove"
        causedBy attrs
        targets attackers \c -> push $ StopAttacking c
      doStep (k - 1) inner
      pure e
    _ -> GuardianOfTheGoldenKing <$> runMessage msg attrs
