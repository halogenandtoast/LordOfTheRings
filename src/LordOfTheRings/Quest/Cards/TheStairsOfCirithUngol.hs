module LordOfTheRings.Quest.Cards.TheStairsOfCirithUngol (theStairsOfCirithUngol) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (cardInVictoryDisplay, discardRandomFromEachHand)
import LordOfTheRings.Trait (Trait (Nazgul, Underground))

newtype TheStairsOfCirithUngol = TheStairsOfCirithUngol QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theStairsOfCirithUngol :: QuestCard TheStairsOfCirithUngol
theStairsOfCirithUngol = quest TheStairsOfCirithUngol Cards.theStairsOfCirithUngol

-- | Progress cannot be placed on Underground locations.
instance HasModifiersFor TheStairsOfCirithUngol where
  getModifiersFor (TheStairsOfCirithUngol a) = when a.flipped do
    underground <- select (LocationWithTrait Underground)
    for_ underground \l -> modified a l [ProgressLimit 0]

{- | Response: After Sméagol's Secret Way is explored, discard 3 random cards from each
player's hand to discard each Nazgûl enemy in the staging area. The cost is paid in full or
not at all: each player must hold 3 cards.
Forced: At the end of the quest phase, if Sméagol's Secret Way is in the victory display,
advance to stage 2A.
-}
instance HasAbilities TheStairsOfCirithUngol where
  getAbilities (TheStairsOfCirithUngol a)
    | a.flipped =
        [ restricted a 1 (NotCriterion $ PlayerExists $ NotPlayer $ PlayerWithCardsInHand 3)
            $ response
            $ ExploredMatchingWindow After (cardIs Locations.smeagolsSecretWay)
        , mkAbility a 2 $ forced $ PhaseEndsWindow When QuestPhase
        ]
    | otherwise = []

{- | Setup: Set Cleft of Cirith Ungol, Shelob and The Great Spider encounter set aside, out
of play (the scenario sets them aside as it gathers). Put Sméagol into play under the first
player's control. Make Morgul Vale the active location. Add Sméagol's Secret Way to the
staging area.
-}
instance RunMessage TheStairsOfCirithUngol where
  runMessage msg q@(TheStairsOfCirithUngol attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      lead <- getLead
      putObjectiveAllyIntoPlayUnder lead =<< genEncounterCard Objectives.smeagolTheLandOfShadow
      traverse_ putIntoPlayAsActiveLocation =<< getSetAside (cardIs Locations.morgulValeShelobsLair)
      addSetAsideToStaging (cardIs Locations.smeagolsSecretWay)
      TheStairsOfCirithUngol <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardRandomFromEachHand 3
      traverse_ discard =<< select (EnemyWithTrait Nazgul <> EnemyInStagingArea)
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      whenM (cardInVictoryDisplay Locations.smeagolsSecretWay) advanceQuest
      pure q
    _ -> TheStairsOfCirithUngol <$> runMessage msg attrs
