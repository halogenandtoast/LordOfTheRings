module LordOfTheRings.Quest.Cards.TheSideDoor (theSideDoor) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheSideDoor = TheSideDoor QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSideDoor :: QuestCard TheSideDoor
theSideDoor = quest TheSideDoor Cards.theSideDoor

instance HasAbilities TheSideDoor where
  getAbilities (TheSideDoor a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When PlanningPhase | a.flipped]

{- | Setup: Add Dagnir to the staging area and set The First Forge aside, out of play. Create
the Caves deck and the quest deck. Attach Wormsbane to a hero, Broken side face-up. Shuffle
the encounter deck and discard cards from the top until X locations have been discarded,
where X is 1 less than the number of players in the game. Add those locations to the
staging area. (The scenario builds both decks and sets the rest aside as it gathers.)

Forced: At the end of the planning phase, the first player looks at the top 2 cards of the
quest deck and chooses 1 to place on the bottom of the quest deck. Then, add The Side Door
to the victory display and advance to the top stage 2A of the quest deck.
-}
instance RunMessage TheSideDoor where
  runMessage msg q@(TheSideDoor attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.dagnir)
      lead <- getLead
      heroes <- select AnyHero
      swords <- getSetAside (cardIs Objectives.wormsbane)
      for_ swords \sword -> chooseOrRunOneM lead do
        questionLabeled "theSideDoor.wormsbane"
        causedBy attrs
        targets heroes $ putObjectiveIntoPlayAttached sword
      shuffleEncounterDeck
      doStep 1 msg
      TheSideDoor <$> runMessage msg attrs
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs (players - 1) (CardWithType LocationType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType LocationType) cards
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      top <- getsGame (take 2 . (.questDeck) . toAttrs . (.scenario))
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "theSideDoor.bottom"
        causedBy attrs
        showing (map EncounterCard top)
        targets top (putOnBottomOfQuestDeck . EncounterCard)
      addToVictoryDisplay attrs
      advanceQuest
      pure q
    _ -> TheSideDoor <$> runMessage msg attrs
