module LordOfTheRings.Quest.Cards.RescueIarion (rescueIarion) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype RescueIarion = RescueIarion QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Time 4, as its card def says.
rescueIarion :: QuestCard RescueIarion
rescueIarion = quest RescueIarion Cards.rescueIarion

-- | Forced: After the last time counter is removed from this stage, discard Iârion: and
-- with him gone, the players lose.
instance HasAbilities RescueIarion where
  getAbilities (RescueIarion a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | When Revealed: The first player loses control of Iârion and places him facedown
-- underneath this stage. When this stage is defeated, the first player takes control of
-- Iârion and exhausts him.
-- Underneath the stage Iârion is under no player's control, as Captured! keeps Faramir.
instance RunMessage RescueIarion where
  runMessage msg q@(RescueIarion attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      iarions <- select (AllyWithCardCode $ toCardCode Objectives.iarion)
      for_ iarions \i -> placeAlly i (AttachedTo $ toTarget attrs)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      iarions <- select (AllyWithCardCode $ toCardCode Objectives.iarion)
      traverse_ discard iarions
      -- under no one's control his own Forced does not answer: his leaving loses it here
      unless (null iarions) $ gameOver Lost
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      lead <- getLead
      iarions <- select (AllyWithCardCode $ toCardCode Objectives.iarion)
      for_ iarions \i -> do
        exhaust i
        takeControl i lead
      RescueIarion <$> runMessage msg attrs
    _ -> RescueIarion <$> runMessage msg attrs
