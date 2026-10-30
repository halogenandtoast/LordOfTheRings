module LordOfTheRings.Treachery.Cards.DeeperIntoTheRuins (deeperIntoTheRuins) where

import LordOfTheRings.Helpers.IslandMap (activePosition, locationsLeftOf)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DeeperIntoTheRuins = DeeperIntoTheRuins TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deeperIntoTheRuins :: TreacheryCard DeeperIntoTheRuins
deeperIntoTheRuins = treachery DeeperIntoTheRuins Cards.deeperIntoTheRuins

-- | When Revealed: Until the end of the phase, add X threat to the total threat in the
-- staging area. X is the total threat of all locations to the left of the active location
-- (in its row). If X is 0, Deeper into the Ruins gains Doomed 1.
instance RunMessage DeeperIntoTheRuins where
  runMessage msg t@(DeeperIntoTheRuins attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      left <- maybe (pure []) locationsLeftOf =<< activePosition
      x <- sum <$> traverse (getLocationThreat . (.id)) left
      if x == 0
        then raiseThreatFromDoomed 1
        else phaseModifier attrs GameTarget (StagingThreatBonus x)
      pure t
    _ -> DeeperIntoTheRuins <$> runMessage msg attrs
