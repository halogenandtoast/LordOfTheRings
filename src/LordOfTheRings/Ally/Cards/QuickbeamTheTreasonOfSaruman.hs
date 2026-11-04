module LordOfTheRings.Ally.Cards.QuickbeamTheTreasonOfSaruman (quickbeamTheTreasonOfSaruman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype QuickbeamTheTreasonOfSaruman = QuickbeamTheTreasonOfSaruman AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Enters play exhausted.
quickbeamTheTreasonOfSaruman :: AllyCard QuickbeamTheTreasonOfSaruman
quickbeamTheTreasonOfSaruman = ally (QuickbeamTheTreasonOfSaruman . (exhaustedL .~ True)) Cards.quickbeamTheTreasonOfSaruman

-- | Cannot have restricted attachments.
instance HasModifiersFor QuickbeamTheTreasonOfSaruman where
  getModifiersFor (QuickbeamTheTreasonOfSaruman a) = modifySelf a [CannotHaveRestrictedAttachments]

-- | Response: After Quickbeam enters play, deal 1 damage to him to ready him.
instance HasAbilities QuickbeamTheTreasonOfSaruman where
  getAbilities (QuickbeamTheTreasonOfSaruman a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage QuickbeamTheTreasonOfSaruman where
  runMessage msg s@(QuickbeamTheTreasonOfSaruman a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      dealDamage a a 1
      ready a
      pure s
    _ -> QuickbeamTheTreasonOfSaruman <$> runMessage msg a
