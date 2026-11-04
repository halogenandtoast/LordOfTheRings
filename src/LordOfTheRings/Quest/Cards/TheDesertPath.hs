module LordOfTheRings.Quest.Cards.TheDesertPath (theDesertPath) where

import LordOfTheRings.EncounterSet (EncounterSet (DesertSands, MountainsOfShadow))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (playedAllies, setAsideOf, shuffleSetAsideSetIntoEncounterDeck)

newtype TheDesertPath = TheDesertPath QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDesertPath :: QuestCard TheDesertPath
theDesertPath = quest TheDesertPath Cards.theDesertPath

{- | When Revealed: Shuffle the Desert Sands encounter set into the encounter deck. Remove
the Mountains of Shadow encounter set and the other stage 2A from the game.
Forced: After a player plays an ally, he must either exhaust that ally or deal 1 damage to
it. An exhausted ally can only take the damage (FAQ 1.44).
Forced: When this stage is defeated, advance to a random stage 3 (the scenario picks it).
-}
instance RunMessage TheDesertPath where
  runMessage msg q@(TheDesertPath attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheDesertPath <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleSetAsideSetIntoEncounterDeck DesertSands
      removeSetAsideFromGame =<< setAsideOf MountainsOfShadow
      removeFromQuestDeck Cards.theEphelDuath
      pure q
    CheckWindows ws | attrs.flipped -> do
      allies <- playedAllies ws
      for_ allies \(aid, pid, ready') -> chooseOrRunOneM pid do
        questionLabeled "theDesertPath.choose"
        causedBy attrs
        when ready' $ labeled "theDesertPath.exhaust" $ exhaust aid
        labeled "theDesertPath.damage" $ dealDamage attrs aid 1
      TheDesertPath <$> runMessage msg attrs
    _ -> TheDesertPath <$> runMessage msg attrs
