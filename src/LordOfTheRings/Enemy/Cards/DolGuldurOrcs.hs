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
      unless (null committed)
        $ push
        $ questionLabelWith (ikey "label.dolGuldurOrcs.damage") (Just $ toTarget attrs) [] lead
          [targetLabel c [DealDamage (toSource attrs) c 2] | c <- committed]
      pure e
    ResolveShadowEffect cid eid _ defender | cid == attrs.cardId -> do
      push $ CreateEffect $ Effect (toSource attrs) (toTarget eid) [AttackModifier $ maybe 3 (const 1) defender] UntilEndOfPhase
      pure e
    _ -> DolGuldurOrcs <$> runMessage msg attrs
