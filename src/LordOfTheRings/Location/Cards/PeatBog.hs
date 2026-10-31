module LordOfTheRings.Location.Cards.PeatBog (peatBog) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype PeatBog = PeatBog LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

peatBog :: LocationCard PeatBog
peatBog = location PeatBog Cards.peatBog

-- | While Peat Bog is in the staging area, it gains: "Forced: After the players advance to
-- a quest stage, each player discards 1 random card from his hand."
instance HasAbilities PeatBog where
  getAbilities (PeatBog a) = [mkAbility a 1 $ forced $ AdvancedToStageWindow After | a.placement == StagingArea]

instance RunMessage PeatBog where
  runMessage msg l@(PeatBog a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> discardRandomFromHand p 1
      pure l
    _ -> PeatBog <$> runMessage msg a
