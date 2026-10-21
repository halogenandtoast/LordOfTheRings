module LordOfTheRings.Ally.Cards.SilvanTracker (silvanTracker) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Silvan))

newtype SilvanTracker = SilvanTracker AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

silvanTracker :: AllyCard SilvanTracker
silvanTracker = ally SilvanTracker Cards.silvanTracker

-- | Response: After a Silvan character readies during the refresh phase, heal 1 damage
-- from that character.
instance HasAbilities SilvanTracker where
  getAbilities (SilvanTracker a) = [mkAbility a 1 $ response $ ReadiedWindow After (Just Silvan)]

instance RunMessage SilvanTracker where
  runMessage msg s@(SilvanTracker a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [c | Window _ (Readied c _) <- windows] \c -> healDamage a c 1
      pure s
    _ -> SilvanTracker <$> runMessage msg a
