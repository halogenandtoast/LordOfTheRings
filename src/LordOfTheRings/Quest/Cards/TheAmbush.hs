module LordOfTheRings.Quest.Cards.TheAmbush (theAmbush) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (getHiddenCards, takeHiddenCard, turnHiddenCardsFaceup)

newtype TheAmbush = TheAmbush QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theAmbush :: QuestCard TheAmbush
theAmbush = quest TheAmbush Cards.theAmbush

-- | At the beginning of the quest phase, each player takes 1 hidden card.
-- At the beginning of the combat phase, each player must either turn each of his hidden
-- cards faceup, or take 1 hidden card (FAQ errata).
instance HasAbilities TheAmbush where
  getAbilities (TheAmbush a) =
    [ mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase
    , mkAbility a 2 $ forced $ PhaseBeginsWindow After CombatPhase
    ]

-- | Setup: Add The Cross-roads and Black Númenorean to the staging area. Put the Faramir
-- and Lord Alcaron objective allies into play. Shuffle the encounter deck and reveal 1
-- card from the encounter deck per player and add it to the staging area.
instance RunMessage TheAmbush where
  runMessage msg q@(TheAmbush attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let setUp = map toCardCode [Locations.theCrossroads, Enemies.blackNumenorean, Enemies.blackNumenoreanNightmare, Objectives.faramirObjective, Objectives.lordAlcaronTheBloodOfGondor]
      -- the objective allies come under the first player's control as they are added
      for_ (filter ((`elem` setUp) . toCardCode) deck) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      shuffleEncounterDeck
      revealEncounterCards . length =<< getRemainingPlayers
      turnOver attrs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer takeHiddenCard
      pure q
    -- one who has no hidden card to turn faceup takes one
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      eachPlayer \pid -> do
        hidden <- getHiddenCards pid
        if null hidden
          then takeHiddenCard pid
          else chooseOneM pid do
            questionLabeled "theAmbush.choose"
            causedBy attrs
            labeled "theAmbush.turnFaceup" $ void $ turnHiddenCardsFaceup pid
            labeled "theAmbush.take" $ takeHiddenCard pid
      pure q
    _ -> TheAmbush <$> runMessage msg attrs
