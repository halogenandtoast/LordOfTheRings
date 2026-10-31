-- | What the cards of the scenarios with Twists and Turns share: the Cave Torch.
module LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (readyCaveTorch, attachCaveTorch, caveTorchInPlay, caveTorchExhausts) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Id (ObjectiveId, PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Matcher.Window (WindowMatcher (ObjectiveExhaustedWindow))
import LordOfTheRings.Prelude
import LordOfTheRings.Timing (Timing (After))
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

{- | Setup: the first player attaches Cave Torch, taken from the encounter deck, to a hero
of his choice, as Entering the Mines, Journey in the Black Pit and The Dripping Walls read.
-}
attachCaveTorch :: (ReverseQueue m, Targetable source) => source -> m ()
attachCaveTorch source = do
  deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
  for_ (find ((== toCardCode Objectives.caveTorch) . toCardCode) deck) \torch -> do
    removeFromEncounterDeck torch
    lead <- getLead
    heroes <- select AnyHero
    chooseOrRunOneM lead do
      questionLabeled "caveTorch.attach"
      causedBy source
      targets heroes (putObjectiveIntoPlayAttached torch)

-- | Cave Torch is in play, ready or not.
caveTorchInPlay :: (HasGame m) => m Bool
caveTorchInPlay = getsGame (any ((== toCardCode Objectives.caveTorch) . toCardCode) . toList . (.entities.objectives))

-- | "After Cave Torch exhausts", as the nightmare cards answer it.
caveTorchExhausts :: WindowMatcher
caveTorchExhausts = ObjectiveExhaustedWindow After (toCardCode Objectives.caveTorch)
