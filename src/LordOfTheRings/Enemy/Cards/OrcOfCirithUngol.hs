module LordOfTheRings.Enemy.Cards.OrcOfCirithUngol (orcOfCirithUngol) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (discardRandomFromEachHand)

newtype OrcOfCirithUngol = OrcOfCirithUngol EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcOfCirithUngol :: EnemyCard OrcOfCirithUngol
orcOfCirithUngol = enemy OrcOfCirithUngol Cards.orcOfCirithUngol

-- | Peril.
-- When Revealed: Either discard 1 random card from each player's hand, or Orc of Cirith
-- Ungol makes an immediate attack against you.
instance RunMessage OrcOfCirithUngol where
  runMessage msg e@(OrcOfCirithUngol attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      -- with every hand empty the discard changes nothing, so the attack must be chosen
      -- (FAQ 1.44)
      cards <- selectAny (PlayerWithCardsInHand 1)
      chooseOrRunOneM you do
        questionLabeled "orcOfCirithUngol.choose"
        causedBy attrs
        when cards $ labeled "orcOfCirithUngol.discard" $ discardRandomFromEachHand 1
        labeled "orcOfCirithUngol.attack" $ immediateAttack you attrs.id
      pure e
    _ -> OrcOfCirithUngol <$> runMessage msg attrs
