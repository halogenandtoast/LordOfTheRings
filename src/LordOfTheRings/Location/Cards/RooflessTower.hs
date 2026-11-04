module LordOfTheRings.Location.Cards.RooflessTower (rooflessTower) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype RooflessTower = RooflessTower LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

rooflessTower :: LocationCard RooflessTower
rooflessTower = location RooflessTower Cards.rooflessTower

-- | Travel: Exhaust a hero to travel here.
instance HasModifiersFor RooflessTower where
  getModifiersFor (RooflessTower a) = do
    stuck <- selectNone ReadyHero
    when stuck $ modifySelf a [CannotBeTraveledTo]

-- | While Roofless Tower is in the staging area, it gains: "Forced: When a player plays an
-- event, either remove 1 progress from the main quest, or cancel the effects of that
-- event." The removal is offered only when it can be made in full (FAQ 1.44).
instance HasAbilities RooflessTower where
  getAbilities (RooflessTower a) = [mkAbility a 1 $ forced $ EventPlayedWindow When | a.placement == StagingArea]

instance RunMessage RooflessTower where
  runMessage msg l@(RooflessTower a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 ws -> do
      for_ [(p, c) | Window _ (PlayingEvent p c) <- ws] \(p, c) ->
        chooseOrRunOneM p do
          questionLabeled "rooflessTower.choose"
          causedBy a
          removeMainQuestProgressLabeled a 1
          -- canceled once chosen: the event's play waits on the game's queue
          labeled "rooflessTower.cancel" $ doStep 1 (UseCardAbility pid (toSource a) 1 [Window When (PlayingEvent p c)])
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 ws) -> do
      for_ [c | Window _ (PlayingEvent _ c) <- ws] cancelEvent
      pure l
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      heroes <- select ReadyHero
      chooseOneM lead do
        questionLabeled "rooflessTower.exhaustHero"
        causedBy a
        targets heroes exhaust
      RooflessTower <$> runMessage msg a
    _ -> RooflessTower <$> runMessage msg a
