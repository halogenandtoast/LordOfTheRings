module LordOfTheRings.Ally.Cards.EscortFromEdoras (escortFromEdoras) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype EscortFromEdoras = EscortFromEdoras AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

escortFromEdoras :: AllyCard EscortFromEdoras
escortFromEdoras = ally EscortFromEdoras Cards.escortFromEdoras

-- | While committed to a quest, Escort from Edoras gets +2 willpower.
instance HasModifiersFor EscortFromEdoras where
  getModifiersFor (EscortFromEdoras a) = when (allyCommitted a) $ modifySelf a [WillpowerModifier 2]

-- | Forced: After resolving a quest to which Escort from Edoras was committed, discard
-- Escort from Edoras from play.
instance RunMessage EscortFromEdoras where
  runMessage msg s@(EscortFromEdoras a) = case msg of
    QuestResolution | allyCommitted a -> do
      discard a
      pure s
    _ -> EscortFromEdoras <$> runMessage msg a
