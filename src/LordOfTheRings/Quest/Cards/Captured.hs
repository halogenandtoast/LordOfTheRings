module LordOfTheRings.Quest.Cards.Captured (captured) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (getHiddenCards, takeHiddenCard, turnHiddenCardsFaceup)

newtype Captured = Captured QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

captured :: QuestCard Captured
captured = quest Captured Cards.captured

-- | At the beginning of the quest phase, each player takes 1 hidden card.
instance HasAbilities Captured where
  getAbilities (Captured a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

-- | When Revealed: Place Faramir and Lord Alcaron under Captured! (they are in play but
-- players cannot gain control of them). Each player turns each of his hidden cards faceup.
-- Battle. If a player has 5 hidden cards, he must turn them faceup: checked each time he
-- takes one. If the players defeat this stage, they have won the game.
instance RunMessage Captured where
  runMessage msg q@(Captured attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      Captured <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      captives <-
        select
          $ AllyMatchAny
            [AllyWithCardCode (toCardCode Objectives.faramirObjective), AllyWithCardCode (toCardCode Objectives.lordAlcaron)]
      for_ captives \a -> placeAlly a (AttachedTo $ toTarget attrs)
      eachPlayer (void . turnHiddenCardsFaceup)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer takeHiddenCard
      pure q
    TakeHiddenCard _ -> do
      doStep 1 msg
      Captured <$> runMessage msg attrs
    DoStep 1 (TakeHiddenCard pid) -> do
      hidden <- getHiddenCards pid
      when (length hidden >= 5) $ void $ turnHiddenCardsFaceup pid
      pure q
    _ -> Captured <$> runMessage msg attrs
