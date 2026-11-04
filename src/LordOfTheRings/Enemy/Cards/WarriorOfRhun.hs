module LordOfTheRings.Enemy.Cards.WarriorOfRhun (warriorOfRhun) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WarriorOfRhun = WarriorOfRhun EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

warriorOfRhun :: EnemyCard WarriorOfRhun
warriorOfRhun = enemy WarriorOfRhun Cards.warriorOfRhun

-- | Forced: When Warrior of Rhûn attacks, attach the top Easterling treachery in the
-- discard pile to it.
instance HasAbilities WarriorOfRhun where
  getAbilities (WarriorOfRhun a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: Attach the top Easterling treachery in the discard pile to attacking enemy.
instance RunMessage WarriorOfRhun where
  runMessage msg e@(WarriorOfRhun attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      attachTopOfEncounterDiscard easterlingTreachery attrs
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      attachTopOfEncounterDiscard easterlingTreachery (EnemyTarget eid)
      pure e
    _ -> WarriorOfRhun <$> runMessage msg attrs
