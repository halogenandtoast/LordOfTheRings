module LordOfTheRings.Quest.Cards.TheEphelDuath (theEphelDuath) where

import LordOfTheRings.EncounterSet (EncounterSet (DesertSands, MountainsOfShadow))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (playedAllies, setAsideOf, shuffleSetAsideSetIntoEncounterDeck)

newtype TheEphelDuath = TheEphelDuath QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEphelDuath :: QuestCard TheEphelDuath
theEphelDuath = quest TheEphelDuath Cards.theEphelDuath

{- | When Revealed: Shuffle the Mountains of Shadow encounter set into the encounter deck.
Remove the Desert Sands encounter set and the other stage 2A from the game.
Forced: After a player plays an ally, he must either exhaust that ally or raise his threat
by 1. A player whose ally is exhausted can only raise his threat (FAQ 1.44).
Forced: When this stage is defeated, advance to a random stage 3 (the scenario picks it).
-}
instance RunMessage TheEphelDuath where
  runMessage msg q@(TheEphelDuath attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheEphelDuath <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleSetAsideSetIntoEncounterDeck MountainsOfShadow
      removeSetAsideFromGame =<< setAsideOf DesertSands
      removeFromQuestDeck Cards.theDesertPath
      pure q
    CheckWindows ws | attrs.flipped -> do
      allies <- playedAllies ws
      for_ allies \(aid, pid, ready') -> chooseOrRunOneM pid do
        questionLabeled "theEphelDuath.choose"
        causedBy attrs
        when ready' $ labeled "theEphelDuath.exhaust" $ exhaust aid
        labeled "theEphelDuath.threat" $ encounterRaisesThreat pid 1
      TheEphelDuath <$> runMessage msg attrs
    _ -> TheEphelDuath <$> runMessage msg attrs
