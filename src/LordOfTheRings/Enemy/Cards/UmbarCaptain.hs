module LordOfTheRings.Enemy.Cards.UmbarCaptain (umbarCaptain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype UmbarCaptain = UmbarCaptain EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarCaptain :: EnemyCard UmbarCaptain
umbarCaptain = enemy UmbarCaptain Cards.umbarCaptain

-- | While you are engaged with Umbar Captain, characters you control cannot attack Ship
-- enemies.
instance HasModifiersFor UmbarCaptain where
  getModifiersFor (UmbarCaptain a) = case a.placement of
    EngagedWith pid -> do
      ships <- select shipEnemy
      for_ ships \s -> modified a s [CannotBeAttackedByCharactersOf pid]
    _ -> pure ()
