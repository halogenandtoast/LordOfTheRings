module LordOfTheRings.Treachery.Cards.FierceAttack (fierceAttack) where

import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (placeAssaultProgress)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FierceAttack = FierceAttack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fierceAttack :: TreacheryCard FierceAttack
fierceAttack = treachery FierceAttack Cards.fierceAttack

-- | Doomed 1.
-- When Revealed: Place X progress on The Goblins' Assault, where X is the stage number of
-- the main quest.
-- Shadow: Attacking enemy gets +X attack, where X is the stage number of the main quest.
instance RunMessage FierceAttack where
  runMessage msg t@(FierceAttack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeAssaultProgress attrs =<< getMainStage
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      x <- getMainStage
      when (x > 0) $ enemyGetsAttack attrs eid x
      pure t
    _ -> FierceAttack <$> runMessage msg attrs
