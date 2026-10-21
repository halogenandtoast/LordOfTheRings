module LordOfTheRings.Location.Cards.ForestGrove (forestGrove) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives

newtype ForestGrove = ForestGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestGrove :: LocationCard ForestGrove
forestGrove = location ForestGrove Cards.forestGrove

-- | Response: After the players explore Forest Grove, search the encounter deck and discard
-- pile for 1 Athelas objective, and add it to the staging area. Then, shuffle the
-- encounter deck.
instance HasAbilities ForestGrove where
  getAbilities (ForestGrove a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

instance RunMessage ForestGrove where
  runMessage msg l@(ForestGrove a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      push $ SearchEncounterDeckAndDiscard pid (toTarget a) (cardIs Objectives.athelas)
      pure l
    _ -> ForestGrove <$> runMessage msg a
