module LordOfTheRings.Ally.Cards.GildorInglorionObjective (gildorInglorionObjective) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype GildorInglorionObjective = GildorInglorionObjective AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon with an encounter card back (A Shadow of the Past's campaign card). The first
-- player gains control of Gildor Inglorion, as its keyword has the game do.
gildorInglorionObjective :: AllyCard GildorInglorionObjective
gildorInglorionObjective = ally GildorInglorionObjective Cards.gildorInglorionObjective

-- | Forced: At the end of the round, discard Gildor Inglorion. The first player may spend 1
-- Fellowship resource to cancel this effect.
instance HasAbilities GildorInglorionObjective where
  getAbilities (GildorInglorionObjective a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage GildorInglorionObjective where
  runMessage msg s@(GildorInglorionObjective a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      fellowship <- getResourcesOfSphere lead Fellowship
      chooseOrRunOneM lead do
        questionLabeled "gildorInglorionObjective.choose"
        causedBy a
        when (fellowship > 0) $ labeled "gildorInglorionObjective.cancel" $ spendResources lead Fellowship 1
        labeled "gildorInglorionObjective.discard" $ discard a a
      pure s
    _ -> GildorInglorionObjective <$> runMessage msg a
