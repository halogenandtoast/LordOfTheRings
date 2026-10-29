module LordOfTheRings.Enemy.Cards.DunlandRaider (dunlandRaider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DunlandRaider = DunlandRaider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlandRaider :: EnemyCard DunlandRaider
dunlandRaider = enemy DunlandRaider Cards.dunlandRaider

-- | Forced: After Dunland Raider engages a player, that player must deal X damage divided
-- among characters he controls where X equals the number of cards in his hand.
instance HasAbilities DunlandRaider where
  getAbilities (DunlandRaider a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Discard an attachment you control.
instance RunMessage DunlandRaider where
  runMessage msg e@(DunlandRaider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> do
        n <- length . (.hand) <$> getPlayer pid
        dealDamageDivided attrs pid n
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard attrs pid attachments
      pure e
    _ -> DunlandRaider <$> runMessage msg attrs
