module LordOfTheRings.Enemy.Cards.ChiefTurchTheDunlandTrap (chiefTurchTheDunlandTrap) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype ChiefTurchTheDunlandTrap = ChiefTurchTheDunlandTrap EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

chiefTurchTheDunlandTrap :: EnemyCard ChiefTurchTheDunlandTrap
chiefTurchTheDunlandTrap = enemy ChiefTurchTheDunlandTrap Cards.chiefTurchTheDunlandTrap

-- | Cannot have attachments. Cannot leave play (his card is indestructible, so he is not
-- destroyed either). Chief Turch engages the first player: no other player can engage
-- him.
instance HasModifiersFor ChiefTurchTheDunlandTrap where
  getModifiersFor (ChiefTurchTheDunlandTrap a) = do
    lead <- getLead
    others <- filter (/= lead) <$> getPlayerOrder
    modified a a $ [CannotHaveAttachments, CannotLeavePlay, EngagesOnly lead] <> map CannotEngagePlayer others

-- | Forced: After an enemy engages a player, remove 1 time counter from the current quest.
instance HasAbilities ChiefTurchTheDunlandTrap where
  getAbilities (ChiefTurchTheDunlandTrap a) = [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After]

-- | As the first player token passes, he engages its new holder; anything else that would
-- have another player engage him has the first player engage him.
instance RunMessage ChiefTurchTheDunlandTrap where
  runMessage msg e@(ChiefTurchTheDunlandTrap attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeTimeCountersFromCurrentQuest attrs 1
      pure e
    SetFirstPlayer pid | EngagedWith p <- attrs.placement, p /= pid -> do
      engageEnemy pid attrs.id
      ChiefTurchTheDunlandTrap <$> runMessage msg attrs
    _ -> ChiefTurchTheDunlandTrap <$> runMessage msg attrs
