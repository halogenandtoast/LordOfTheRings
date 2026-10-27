-- | What Into the Pit's cards share: the Cave Torch.
module LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (readyCaveTorch) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Id (ObjectiveId, PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | A ready Cave Torch on a hero, and the player controlling that hero, who may exhaust it.
readyCaveTorch :: (HasGame m) => m (Maybe (ObjectiveId, PlayerId))
readyCaveTorch = do
  torches <-
    getsGame
      $ filter (\o -> toCardCode o == toCardCode Objectives.caveTorch && not o.exhausted)
      . toList
      . (.entities.objectives)
  held <- for torches \o -> case o.placement of
    AttachedTo (HeroTarget hid) -> Just . (toId o,) . (.owner) <$> getHero hid
    _ -> pure Nothing
  pure $ headMay (catMaybes held)
