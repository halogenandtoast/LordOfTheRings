module LordOfTheRings.Ally.Cards.DunedainLookout (dunedainLookout) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype DunedainLookout = DunedainLookout AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainLookout :: AllyCard DunedainLookout
dunedainLookout = ally DunedainLookout Cards.dunedainLookout

-- | Response: Discard Dúnedain Lookout to cancel the 'When Revealed' effects of an enemy
-- just revealed from the encounter deck.
instance HasAbilities DunedainLookout where
  getAbilities (DunedainLookout a) = [restricted a 1 EncounterEffectsCancelable $ response $ EnemyCardRevealedWindow When]

instance RunMessage DunedainLookout where
  runMessage msg s@(DunedainLookout a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      discard a a
      for_ [t | Window _ (Revealed t@(EnemyTarget _)) <- windows] (cancelWhenRevealed pid)
      pure s
    _ -> DunedainLookout <$> runMessage msg a
