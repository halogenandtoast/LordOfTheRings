module LordOfTheRings.Event.Cards.HoldTheLine (holdTheLine) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype HoldTheLine = HoldTheLine EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: When an enemy attacks a player, that player may declare up to 3 eligible
-- characters as defenders against this attack. Then, the players as a group may spend 2
-- tactics resources to ready each defending character that takes no damage from this
-- attack.
holdTheLine :: EventCard HoldTheLine
holdTheLine = eventWith HoldTheLine Cards.holdTheLine (windowL ?~ AnyEnemyAttacksWindow When)

instance RunMessage HoldTheLine where
  runMessage msg e@(HoldTheLine attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      for_ (take 1 [p | Window _ (EnemyAttacking _ p) <- windows]) \p -> do
        attackModifier attrs (PlayerTarget p) (MayDeclareUpToDefenders 3)
        groupMaySpend pid attrs Tactics 2 $ attackModifier attrs GameTarget ReadyUndamagedDefenders
      pure e
    _ -> HoldTheLine <$> runMessage msg attrs
