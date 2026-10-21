module LordOfTheRings.Ally.Cards.DunedainWatcher (dunedainWatcher) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype DunedainWatcher = DunedainWatcher AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainWatcher :: AllyCard DunedainWatcher
dunedainWatcher = ally DunedainWatcher Cards.dunedainWatcher

-- | Response: Discard Dúnedain Watcher from play to cancel the shadow effects of a card
-- just triggered.
instance HasAbilities DunedainWatcher where
  getAbilities (DunedainWatcher a) = [restricted a 1 EncounterEffectsCancelable $ response $ ShadowEffectWindow When]

instance RunMessage DunedainWatcher where
  runMessage msg s@(DunedainWatcher a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      discard a
      cancelShadowEffects windows
      pure s
    _ -> DunedainWatcher <$> runMessage msg a
