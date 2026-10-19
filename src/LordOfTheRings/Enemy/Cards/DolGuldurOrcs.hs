module LordOfTheRings.Enemy.Cards.DolGuldurOrcs (dolGuldurOrcs) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DolGuldurOrcs = DolGuldurOrcs EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dolGuldurOrcs :: EnemyCard DolGuldurOrcs
dolGuldurOrcs = enemy DolGuldurOrcs Cards.dolGuldurOrcs

-- | When Revealed: The first player chooses 1 character currently committed to a quest.
-- Deal 2 damage to that character.
-- Shadow: attacking enemy gets +1 attack. (+3 attack instead if this attack is undefended.)
instance RunMessage DolGuldurOrcs where
  runMessage msg e@(DolGuldurOrcs attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      committed <- selectCharacters CommittedHero CommittedAlly
      chooseOneM lead do
        questionLabeled "dolGuldurOrcs.damage"
        causedBy attrs
        targets committed \c -> dealDamage attrs c 2
      pure e
    ResolveShadowEffect cid eid _ defender | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid (ifUndefended defender 3 1)
      pure e
    _ -> DolGuldurOrcs <$> runMessage msg attrs
