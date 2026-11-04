module LordOfTheRings.Quest.Cards.TheForbiddenDoor (theForbiddenDoor) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (overcomeByFearOn)

newtype TheForbiddenDoor = TheForbiddenDoor QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theForbiddenDoor :: QuestCard TheForbiddenDoor
theForbiddenDoor = quest TheForbiddenDoor Cards.theForbiddenDoor

-- | Forced: At the end of the planning phase, discard all cards in each player's hand and
-- advance to stage 2A.
instance HasAbilities TheForbiddenDoor where
  getAbilities (TheForbiddenDoor a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When PlanningPhase | a.flipped]

{- | Setup: Set The Stone of Erech and Army of the Dead aside, out of play. Each player
attaches a copy of Overcome by Fear to his threat dial. Remove each unattached copy of
Overcome by Fear from the game. (The scenario sets the first two aside as it gathers; a
player with a copy from the campaign pool on his dial already takes none, Limit 1 per
threat dial, and of the 4 copies only those left are attached.)

When Revealed: Each player may add 1 resource to the resource pool of each hero he
controls. Each player who does so must raise his threat by 3.
-}
instance RunMessage TheForbiddenDoor where
  runMessage msg q@(TheForbiddenDoor attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      withCopy <- filterM (fmap notNull . overcomeByFearOn) players
      let copies = 4 - length withCopy
      for_ (take copies $ filter (`notElem` withCopy) players) \p -> do
        card <- genEncounterCard Objectives.overcomeByFear
        putObjectiveIntoPlayAttached card (PlayerTarget p)
      TheForbiddenDoor <$> runMessage msg attrs
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheForbiddenDoor <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        chooseOneM p do
          questionLabeled "theForbiddenDoor.choose"
          causedBy attrs
          labeled "theForbiddenDoor.resources" do
            for_ heroes \h -> placeResources attrs h 1
            encounterRaisesThreat p 3
          labeled "pass" $ pure ()
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer discardHand
      advanceQuest
      pure q
    _ -> TheForbiddenDoor <$> runMessage msg attrs
